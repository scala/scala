/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.jvm

import java.nio.ByteBuffer

import scala.collection.{ mutable, immutable }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.symtab._
import scala.tools.nsc.symtab.classfile.ClassfileConstants._

import ch.epfl.lamp.fjbg._
import JAccessFlags._
import JObjectType.{ JAVA_LANG_STRING, JAVA_LANG_OBJECT }
import java.io.{ DataOutputStream }
import reflect.generic.{ PickleFormat, PickleBuffer }

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 *
 */
abstract class GenJVM extends SubComponent with GenJVMUtil with GenAndroid {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    NullClass, RuntimeNullClass, NothingClass, RuntimeNothingClass,
    AnyClass, ObjectClass, ThrowsClass, ThrowableClass, ClassfileAnnotationClass,
    SerializableClass, StringClass, ClassClass, FunctionClass,
    DeprecatedAttr, SerializableAttr, SerialVersionUIDAttr, VolatileAttr,
    TransientAttr, CloneableAttr, RemoteAttr,
    getPrimitiveCompanion
  }

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase): Phase = new JvmPhase(p)

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    override def erasedTypes = true

    override def run {
      // we reinstantiate the bytecode generator at each run, to allow the GC
      // to collect everything
      val codeGenerator = new BytecodeGenerator
      if (settings.debug.value) inform("[running phase " + name + " on icode]")
      if (settings.Xdce.value)
        for ((sym, cls) <- icodes.classes if inliner.isClosureClass(sym) && !deadCode.liveClosures(sym))
          icodes.classes -= sym

      classes.values foreach codeGenerator.genClass
      classes.clear
    }

    def apply(cls: IClass) {
      error("no implementation")
    }
  }

  /** Return the suffix of a class name */
  def moduleSuffix(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod &&
       !sym.isImplClass && !sym.isJavaDefined) "$"
    else ""

  var pickledBytes = 0 // statistics

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator extends BytecodeUtil {
    def debugLevel = settings.debuginfo.indexOfChoice
    import scala.tools.reflect.SigParser
    def verifySig(sym: Symbol, sig: String) = {
      val ok =
        if (sym.isMethod) SigParser verifyMethod sig
        else if (sym.isTerm) SigParser verifyType sig
        else SigParser verifyClass sig

      def label = if (ok) "[ OK ] " else "[BAD!] "
      if (settings.verbose.value || !ok)
        Console.println(label + sym + " in " + sym.owner.skipPackageObject.fullName + "\n  " + sig)
    }

    val MIN_SWITCH_DENSITY = 0.7
    val INNER_CLASSES_FLAGS =
      (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED | ACC_STATIC | ACC_FINAL | ACC_INTERFACE | ACC_ABSTRACT)

    val PublicStatic      = ACC_PUBLIC | ACC_STATIC
    val PublicStaticFinal = ACC_PUBLIC | ACC_STATIC | ACC_FINAL

    val StringBuilderClassName = definitions.StringBuilderClass.fullName
    val BoxesRunTime = "scala.runtime.BoxesRunTime"

    val StringBuilderType = new JObjectType(StringBuilderClassName)
    val toStringType      = new JMethodType(JAVA_LANG_STRING, JType.EMPTY_ARRAY)
    val arrayCloneType    = new JMethodType(JAVA_LANG_OBJECT, JType.EMPTY_ARRAY)
    val MethodTypeType    = new JObjectType("java.dyn.MethodType")
    val JavaLangClassType = new JObjectType("java.lang.Class")
    val MethodHandleType  = new JObjectType("java.dyn.MethodHandle")

    // Scala attributes
    val BeanInfoAttr     = definitions.getClass("scala.reflect.BeanInfo")
    val BeanInfoSkipAttr = definitions.getClass("scala.reflect.BeanInfoSkip")
    val BeanDisplayNameAttr = definitions.getClass("scala.reflect.BeanDisplayName")
    val BeanDescriptionAttr = definitions.getClass("scala.reflect.BeanDescription")

    lazy val CloneableClass  = definitions.getClass("java.lang.Cloneable")
    lazy val RemoteInterface = definitions.getClass("java.rmi.Remote")
    lazy val RemoteException = definitions.getClass("java.rmi.RemoteException").tpe

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    var clasz: IClass = _
    var method: IMethod = _
    var jclass: JClass = _
    var jmethod: JMethod = _
//    var jcode: JExtendedCode = _

    // referenced inner classes
    var innerClasses: immutable.Set[Symbol] = immutable.ListSet.empty

    val fjbgContext = new FJBGContext(49, 0)

    val emitSource = debugLevel >= 1
    val emitLines  = debugLevel >= 2
    val emitVars   = debugLevel >= 3

    /** Write a class to disk, adding the Scala signature (pickled type
     *  information) and inner classes.
     *
     * @param jclass The FJBG class, where code was emitted
     * @param sym    The corresponding symbol, used for looking up pickled information
     */
    def emitClass(jclass: JClass, sym: Symbol) {
      addInnerClasses(jclass)
      val outfile = getFile(sym, jclass, ".class")
      val outstream = new DataOutputStream(outfile.bufferedOutput)
      jclass writeTo outstream
      outstream.close()
      informProgress("wrote " + outfile)
    }

    /** Returns the ScalaSignature annotation if it must be added to this class,
     *  none otherwise; furthermore, it adds to `jclass` the ScalaSig marker
     *  attribute (marking that a scala signature annotation is present) or the
     *  Scala marker attribute (marking that the signature for this class is in
     *  another file). The annotation that is returned by this method must be
     *  added to the class' annotations list when generating them.
     *
     *  @param jclass The class file that is being readied.
     *  @param sym    The symbol for which the signature has been entered in
     *                the symData map. This is different than the symbol
     *                that is being generated in the case of a mirror class.
     *  @return       An option that is:
     *                - defined and contains an annotation info of the
     *                  ScalaSignature type, instantiated with the pickle
     *                  signature for sym (a ScalaSig marker attribute has
     *                  been written);
     *                - undefined if the jclass/sym couple must not contain a
     *                  signature (a Scala marker attribute has been written).
     */
    def scalaSignatureAddingMarker(jclass: JClass, sym: Symbol): Option[AnnotationInfo] =
      currentRun.symData get sym match {
        case Some(pickle) if !jclass.getName().endsWith("$") =>
          val scalaAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaSignatureATTR.toString,
                                        versionPickle.bytes, versionPickle.writeIndex)
          jclass addAttribute scalaAttr
          val scalaAnnot = {
            val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
            AnnotationInfo(sigBytes.sigAnnot, Nil, List((nme.bytes, sigBytes)))
          }
          pickledBytes = pickledBytes + pickle.writeIndex
          currentRun.symData -= sym
          currentRun.symData -= sym.companionSymbol
          Some(scalaAnnot)
        case _ =>
          val markerAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaATTR.toString, new Array[Byte](0), 0)
          jclass addAttribute markerAttr
          None
      }

    var serialVUID: Option[Long] = None
    var isRemoteClass: Boolean = false
    var isParcelableClass = false

    def genClass(c: IClass) {
      val needsEnclosingMethod: Boolean =
        c.symbol.isClass && (c.symbol.originalEnclosingMethod != NoSymbol)

      clasz = c
      innerClasses = immutable.ListSet.empty

      var parents = c.symbol.info.parents
      var ifaces  = JClass.NO_INTERFACES
      val name    = javaName(c.symbol)
      serialVUID  = None
      isRemoteClass = false
      isParcelableClass = isAndroidParcelableClass(c.symbol)

      if (parents.isEmpty)
        parents = List(ObjectClass.tpe)

      for (annot <- c.symbol.annotations) annot match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == SerializableAttr =>
          parents = parents ::: List(SerializableClass.tpe)
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == CloneableAttr =>
          parents = parents ::: List(CloneableClass.tpe)
        case AnnotationInfo(tp, Literal(const) :: _, _) if tp.typeSymbol == SerialVersionUIDAttr =>
          serialVUID = Some(const.longValue)
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == RemoteAttr =>
          parents = parents ::: List(RemoteInterface.tpe)
          isRemoteClass = true
        case _ =>
      }

      parents = parents.distinct

      if (parents.tail.nonEmpty)
        ifaces = parents drop 1 map (x => javaName(x.typeSymbol)) toArray;

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(parents(0).typeSymbol),
                                  ifaces,
                                  c.cunit.source.toString)

      if (isStaticModule(c.symbol) || serialVUID != None || isParcelableClass ||
          clasz.bootstrapClass.isDefined) {
        if (isStaticModule(c.symbol))
          addModuleInstanceField
        addStaticInit(jclass, c.lookupStaticCtor)

        if (isTopLevelModule(c.symbol)) {
          if (c.symbol.companionClass == NoSymbol)
            dumpMirrorClass(c.symbol, c.cunit.source.toString)
          else
            log("No mirror class for module with linked class: " +
                c.symbol.fullName)
        }
      }
      else {
        c.lookupStaticCtor foreach (constructor => addStaticInit(jclass, Some(constructor)))

        // it must be a top level class (name contains no $s)
        def isCandidateForForwarders(sym: Symbol): Boolean =
          atPhase(currentRun.picklerPhase.next) {
            !(sym.name.toString contains '$') && sym.hasModuleFlag && !sym.isImplClass && !sym.isNestedClass
          }

        val lmoc = c.symbol.companionModule
        // add static forwarders if there are no name conflicts; see bugs #363 and #1735
        if (lmoc != NoSymbol && !c.symbol.isInterface) {
          if (isCandidateForForwarders(lmoc) && !settings.noForwarders.value) {
            log("Adding static forwarders from '%s' to implementations in '%s'".format(c.symbol, lmoc))
            addForwarders(jclass, lmoc.moduleClass)
          }
        }
      }

      if (clasz.bootstrapClass.isDefined)
        jclass setBootstrapClass clasz.bootstrapClass.get

      clasz.fields foreach genField
      clasz.methods foreach genMethod

      val ssa = scalaSignatureAddingMarker(jclass, c.symbol)
      addGenericSignature(jclass, c.symbol, c.symbol.owner)
      addAnnotations(jclass, c.symbol.annotations ++ ssa)
      if (needsEnclosingMethod) addEnclosingMethodAttribute(jclass, c.symbol)
      emitClass(jclass, c.symbol)

      if (c.symbol hasAnnotation BeanInfoAttr)
        genBeanInfoClass(c)
    }

    def addEnclosingMethodAttribute(jclass: JClass, clazz: Symbol) {
      val sym = clazz.originalEnclosingMethod
      if (sym.isMethod) {
        log("enclosing method for %s is %s".format(clazz, sym))
        var outerName = javaName(sym.enclClass)
        jclass addAttribute fjbgContext.JEnclosingMethodAttribute(jclass, outerName, javaName(sym), javaType(sym))
      }
    }

    /**
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     */
    def genBeanInfoClass(c: IClass) {
      val description = c.symbol.annotations.find(_.atp.typeSymbol == BeanDescriptionAttr)
      // informProgress(description.toString)

      val beanInfoClass = fjbgContext.JClass(javaFlags(c.symbol),
            javaName(c.symbol) + "BeanInfo",
            "scala/reflect/ScalaBeanInfo",
            JClass.NO_INTERFACES,
            c.cunit.source.toString)

      var fieldList = List[String]()
      for (f <- clasz.fields if f.symbol.hasGetter;
	         val g = f.symbol.getter(c.symbol);
	         val s = f.symbol.setter(c.symbol);
	         if g.isPublic && !(f.symbol.name startsWith "$"))  // inserting $outer breaks the bean
        fieldList = javaName(f.symbol) :: javaName(g) :: (if (s != NoSymbol) javaName(s) else null) :: fieldList
      val methodList =
	     for (m <- clasz.methods
	         if !m.symbol.isConstructor &&
	         m.symbol.isPublic &&
	         !(m.symbol.name startsWith "$") &&
	         !m.symbol.isGetter &&
	         !m.symbol.isSetter) yield javaName(m.symbol)

      val constructor = beanInfoClass.addNewMethod(ACC_PUBLIC, "<init>", JType.VOID, javaTypes(Nil), javaNames(Nil))
      val jcode = constructor.getCode().asInstanceOf[JExtendedCode]
      val strKind = new JObjectType(javaName(StringClass))
      val stringArrayKind = new JArrayType(strKind)
      val conType = new JMethodType(JType.VOID, Array(javaType(ClassClass), stringArrayKind, stringArrayKind))

      def push(lst:Seq[String]) {
        var fi = 0
        for (f <- lst) {
          jcode.emitDUP()
          jcode emitPUSH fi
          if (f != null)
            jcode emitPUSH f
          else
            jcode.emitACONST_NULL()
          jcode emitASTORE strKind
          fi += 1
        }
      }

      jcode.emitALOAD_0()
      // push the class
      jcode emitPUSH javaType(c.symbol).asInstanceOf[JReferenceType]

      // push the the string array of field information
      jcode emitPUSH fieldList.length
      jcode emitANEWARRAY strKind
      push(fieldList)

      // push the string array of method information
      jcode emitPUSH methodList.length
      jcode emitANEWARRAY strKind
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      jcode.emitINVOKESPECIAL("scala/reflect/ScalaBeanInfo", "<init>", conType)
      jcode.emitRETURN()

      // write the bean information class file.
      val outfile = getFile(c.symbol, beanInfoClass, ".class")
      val outstream = new DataOutputStream(outfile.bufferedOutput)
      beanInfoClass writeTo outstream
      outstream.close()
      informProgress("wrote BeanInfo " + outfile)
    }

    /** Add the given 'throws' attributes to jmethod */
    def addExceptionsAttribute(jmethod: JMethod, excs: List[AnnotationInfo]) {
      if (excs.isEmpty) return

      val cpool = jmethod.getConstantPool
      val buf: ByteBuffer = ByteBuffer.allocate(512)
      var nattr = 0

      // put some random value; the actual number is determined at the end
      buf putShort 0xbaba.toShort

      for (AnnotationInfo(tp, List(exc), _) <- excs.distinct if tp.typeSymbol == ThrowsClass) {
        val Literal(const) = exc
        buf.putShort(
          cpool.addClass(
            javaName(const.typeValue.typeSymbol)).shortValue)
        nattr += 1
      }

      assert(nattr > 0)
      buf.putShort(0, nattr.toShort)
      addAttribute(jmethod, tpnme.ExceptionsATTR, buf)
    }

    /** Whether an annotation should be emitted as a Java annotation
     *   .initialize: if 'annot' is read from pickle, atp might be un-initialized
     */
    private def shouldEmitAnnotation(annot: AnnotationInfo) =
      annot.atp.typeSymbol.initialize.isJavaDefined &&
      annot.atp.typeSymbol.isNonBottomSubClass(ClassfileAnnotationClass) &&
      annot.args.isEmpty

    private def emitJavaAnnotations(cpool: JConstantPool, buf: ByteBuffer, annotations: List[AnnotationInfo]): Int = {
      def emitArgument(arg: ClassfileAnnotArg): Unit = arg match {
        case LiteralAnnotArg(const) =>
          const.tag match {
            case BooleanTag =>
              buf put 'Z'.toByte
              buf putShort cpool.addInteger(if(const.booleanValue) 1 else 0).toShort
            case ByteTag    =>
              buf put 'B'.toByte
              buf putShort cpool.addInteger(const.byteValue).toShort
            case ShortTag   =>
              buf put 'S'.toByte
              buf putShort cpool.addInteger(const.shortValue).toShort
            case CharTag    =>
              buf put 'C'.toByte
              buf putShort cpool.addInteger(const.charValue).toShort
            case IntTag     =>
              buf put 'I'.toByte
              buf putShort cpool.addInteger(const.intValue).toShort
            case LongTag    =>
              buf put 'J'.toByte
              buf putShort cpool.addLong(const.longValue).toShort
            case FloatTag   =>
              buf put 'F'.toByte
              buf putShort cpool.addFloat(const.floatValue).toShort
            case DoubleTag  =>
              buf put 'D'.toByte
              buf putShort cpool.addDouble(const.doubleValue).toShort
            case StringTag  =>
              buf put 's'.toByte
              buf putShort cpool.addUtf8(const.stringValue).toShort
            case ClassTag   =>
              buf put 'c'.toByte
              buf putShort cpool.addUtf8(javaType(const.typeValue).getSignature()).toShort
            case EnumTag =>
              buf put 'e'.toByte
              buf putShort cpool.addUtf8(javaType(const.tpe).getSignature()).toShort
              buf putShort cpool.addUtf8(const.symbolValue.name.toString).toShort
          }

        case sb@ScalaSigBytes(bytes) if !sb.isLong =>
          buf put 's'.toByte
          buf putShort cpool.addUtf8(sb.encodedBytes).toShort

        case sb@ScalaSigBytes(bytes) if sb.isLong =>
          buf put '['.toByte
          val stringCount = (sb.encodedBytes.length / 65534) + 1
          buf putShort stringCount.toShort
          for (i <- 0 until stringCount) {
            buf put 's'.toByte
            val j = i * 65535
            val string = sb.encodedBytes.slice(j, j + 65535)
            buf putShort cpool.addUtf8(string).toShort
          }

        case ArrayAnnotArg(args) =>
          buf put '['.toByte
          buf putShort args.length.toShort
          args foreach emitArgument

        case NestedAnnotArg(annInfo) =>
          buf put '@'.toByte
          emitAnnotation(annInfo)
      }

      def emitAnnotation(annotInfo: AnnotationInfo) {
        val AnnotationInfo(typ, args, assocs) = annotInfo
        val jtype = javaType(typ)
        buf putShort cpool.addUtf8(jtype.getSignature()).toShort
        assert(args.isEmpty, args.toString)
        buf putShort assocs.length.toShort
        for ((name, value) <- assocs) {
          buf putShort cpool.addUtf8(name.toString).toShort
          emitArgument(value)
        }
      }

      var nannots = 0
      val pos = buf.position()

      // put some random value; the actual number of annotations is determined at the end
      buf putShort 0xbaba.toShort

      for (annot <- annotations if shouldEmitAnnotation(annot)) {
        nannots += 1
        emitAnnotation(annot)
      }

      // save the number of annotations
      buf.putShort(pos, nannots.toShort)
      nannots
    }

    // @M don't generate java generics sigs for (members of) implementation
    // classes, as they are monomorphic (TODO: ok?)
    private def noGenericSignature(sym: Symbol) = (
      // PP: This condition used to include sym.hasExpandedName, but this leads
      // to the total loss of generic information if a private member is
      // accessed from a closure: both the field and the accessor were generated
      // without it.  This is particularly bad because the availability of
      // generic information could disappear as a consequence of a seemingly
      // unrelated change.
         sym.isSynthetic
      || sym.isLiftedMethod
      || (sym.ownerChain exists (_.isImplClass))
    )
    def addGenericSignature(jmember: JMember, sym: Symbol, owner: Symbol) {
      if (noGenericSignature(sym)) ()
      else {
        val memberTpe = atPhase(currentRun.erasurePhase)(owner.thisType.memberInfo(sym))
        // println("addGenericSignature sym: " + sym.fullName + " : " + memberTpe + " sym.info: " + sym.info)
        // println("addGenericSignature: "+ (sym.ownerChain map (x => (x.name, x.isImplClass))))
        erasure.javaSig(sym, memberTpe) foreach { sig =>
          if (settings.Yverifysigs.value)
            verifySig(sym, sig)

          val index = jmember.getConstantPool.addUtf8(sig).toShort
          if (settings.debug.value && settings.verbose.value)
            atPhase(currentRun.erasurePhase) {
              println("add generic sig "+sym+":"+sym.info+" ==> "+sig+" @ "+index)
            }
          val buf = ByteBuffer.allocate(2)
          buf putShort index
          addAttribute(jmember, tpnme.SignatureATTR, buf)
        }
      }
    }

    def addAnnotations(jmember: JMember, annotations: List[AnnotationInfo]) {
      if (annotations.exists(_.atp.typeSymbol == definitions.DeprecatedAttr)) {
        val attr = jmember.getContext().JOtherAttribute(
          jmember.getJClass(), jmember, tpnme.DeprecatedATTR.toString,
          new Array[Byte](0), 0)
        jmember addAttribute attr
      }

      val toEmit = annotations filter shouldEmitAnnotation
      if (toEmit.isEmpty) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)
      emitJavaAnnotations(jmember.getConstantPool, buf, toEmit)
      addAttribute(jmember, tpnme.RuntimeAnnotationATTR, buf)
    }

    def addParamAnnotations(jmethod: JMethod, pannotss: List[List[AnnotationInfo]]) {
      val annotations = pannotss map (_ filter shouldEmitAnnotation)
      if (annotations forall (_.isEmpty)) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      // number of parameters
      buf.put(annotations.length.toByte)
      for (annots <- annotations)
        emitJavaAnnotations(jmethod.getConstantPool, buf, annots)

      addAttribute(jmethod, tpnme.RuntimeParamAnnotationATTR, buf)
    }

    def addAttribute(jmember: JMember, name: Name, buf: ByteBuffer) {
      if (buf.position() < 2)
        return

      val length = buf.position()
      val arr = buf.array().slice(0, length)

      val attr = jmember.getContext().JOtherAttribute(jmember.getJClass(),
                                                      jmember,
                                                      name.toString,
                                                      arr,
                                                      length)
      jmember addAttribute attr
    }

    def addInnerClasses(jclass: JClass) {
      def addOwnInnerClasses(cls: Symbol): Unit =
        innerClasses ++= (cls.info.decls filter (_.isClass))

      /** The outer name for this inner class. Note that it returns null
       *  when the inner class should not get an index in the constant pool.
       *  That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
       */
      def outerName(innerSym: Symbol): String = {
        if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction || innerSym.originalEnclosingMethod != NoSymbol)
          null
        else {
          var outerName = javaName(innerSym.rawowner)
          // remove the trailing '$'
          if (outerName.endsWith("$") && isTopLevelModule(innerSym.rawowner))
            outerName = outerName dropRight 1
          outerName
        }
      }

      def innerName(innerSym: Symbol): String =
        if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction)
          null
        else
          innerSym.rawname.toString

      // add inner classes which might not have been referenced yet
      atPhase(currentRun.erasurePhase.next) {
        addOwnInnerClasses(clasz.symbol)
        addOwnInnerClasses(clasz.symbol.linkedClassOfClass)
      }

      if (!innerClasses.isEmpty) {
        val innerClassesAttr = jclass.getInnerClasses()
        // sort them so inner classes succeed their enclosing class
        // to satisfy the Eclipse Java compiler
        //for (innerSym <- innerClasses.toList sortBy (_.name.length)) {
        for (innerSym <- innerClasses.toList sortBy (_.name.length);
          outer = outerName(innerSym) if outer != null) {
          var flags = javaFlags(innerSym)
          if (innerSym.rawowner.hasModuleFlag)
            flags |= ACC_STATIC

          innerClassesAttr.addEntry(javaName(innerSym),
              outer, //outerName(innerSym),
              innerName(innerSym),
              (flags & INNER_CLASSES_FLAGS))
        }
      }
    }

    def genField(f: IField) {
      if (settings.debug.value)
        log("Adding field: " + f.symbol.fullName)

      val attributes = f.symbol.annotations.map(_.atp.typeSymbol).foldLeft(0) {
        case (res, TransientAttr) => res | ACC_TRANSIENT
        case (res, VolatileAttr)  => res | ACC_VOLATILE
        case (res, _)             => res
      }

      var flags = javaFlags(f.symbol)
      if (!f.symbol.isMutable)
        flags |= ACC_FINAL

      val jfield =
        jclass.addNewField(flags | attributes,
                           javaName(f.symbol),
                           javaType(f.symbol.tpe))

      addGenericSignature(jfield, f.symbol, clasz.symbol)
      addAnnotations(jfield, f.symbol.annotations)
    }

    def genMethod(m: IMethod) {
      if (m.symbol.isStaticConstructor) return

      log("Generating method " + m.symbol.fullName)
      method = m
      endPC.clear
      computeLocalVarsIndex(m)

      var resTpe = javaType(m.symbol.tpe.resultType)
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID

      var flags = javaFlags(m.symbol)
      if (jclass.isInterface)
        flags |= ACC_ABSTRACT

      if (m.symbol.isStrictFP)
        flags |= ACC_STRICT

      // native methods of objects are generated in mirror classes
      if (method.native)
        flags |= ACC_NATIVE

      jmethod = jclass.addNewMethod(flags,
                                    javaName(m.symbol),
                                    resTpe,
                                    javaTypes(m.params map (_.kind)),
                                    javaNames(m.params map (_.sym)))

      addRemoteException(jmethod, m.symbol)

      if (!jmethod.isAbstract() && !method.native) {
        val jcode = jmethod.getCode().asInstanceOf[JExtendedCode]

        // add a fake local for debugging purpuses
        if (emitVars && isClosureApply(method.symbol)) {
          val outerField = clasz.symbol.info.decl(nme.OUTER_LOCAL)
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this = new Local(
              method.symbol.newVariable(NoPosition, nme.FAKE_LOCAL_THIS), toTypeKind(outerField.tpe), false)
            m.locals = m.locals ::: List(_this)
            computeLocalVarsIndex(m) // since we added a new local, we need to recompute indexes

            jcode.emitALOAD_0()
            jcode.emitGETFIELD(javaName(clasz.symbol),
                               javaName(outerField),
                               javaType(outerField))
            jcode.emitSTORE(indexOf(_this), javaType(_this.kind))
          }
        }

        for (local <- m.locals if ! m.params.contains(local)) {
          if (settings.debug.value)
            log("add local var: " + local)
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym))
        }

        genCode(m)
        if (emitVars)
          genLocalVariableTable(m, jcode);
      }

      addGenericSignature(jmethod, m.symbol, clasz.symbol)
      val (excs, others) = splitAnnotations(m.symbol.annotations, ThrowsClass)
      addExceptionsAttribute(jmethod, excs)
      addAnnotations(jmethod, others)
      addParamAnnotations(jmethod, m.params.map(_.sym.annotations))
    }

    private def addRemoteException(jmethod: JMethod, meth: Symbol) {
      def isRemoteThrows(ainfo: AnnotationInfo) = ainfo match {
        case AnnotationInfo(tp, List(arg), _) if tp.typeSymbol == ThrowsClass =>
          arg match {
            case Literal(Constant(tpe: Type)) if tpe.typeSymbol == RemoteException.typeSymbol => true
            case _ => false
          }
        case _ => false
      }

      if (isRemoteClass ||
          (meth.hasAnnotation(RemoteAttr) && jmethod.isPublic)) {
        val c = Constant(RemoteException)
        val ainfo = AnnotationInfo(ThrowsClass.tpe, List(Literal(c).setType(c.tpe)), List())
        if (!meth.annotations.exists(isRemoteThrows)) {
          meth addAnnotation ainfo
        }
      }
    }


    /** Return a pair of lists of annotations, first one containing all
     *  annotations for the given symbol, and the rest.
     */
    private def splitAnnotations(annotations: List[AnnotationInfo], annotSym: Symbol): (List[AnnotationInfo], List[AnnotationInfo]) = {
      annotations.partition { a => a match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == annotSym => true
        case _ => false
      }}
    }

    private def isClosureApply(sym: Symbol): Boolean = {
      (sym.name == nme.apply) &&
      sym.owner.isSynthetic &&
      sym.owner.tpe.parents.exists { t =>
        val TypeRef(_, sym, _) = t
        FunctionClass contains sym
      }
    }

    def addModuleInstanceField {
      jclass.addNewField(PublicStaticFinal,
                        nme.MODULE_INSTANCE_FIELD.toString,
                        jclass.getType())
    }

    def addStaticInit(cls: JClass, mopt: Option[IMethod]) {
      val clinitMethod = cls.addNewMethod(PublicStatic,
                                          "<clinit>",
                                          JType.VOID,
                                          JType.EMPTY_ARRAY,
                                          new Array[String](0))
      val clinit = clinitMethod.getCode().asInstanceOf[JExtendedCode]

      mopt match {
       	case Some(m) =>
          if (clasz.bootstrapClass.isDefined) legacyEmitBootstrapMethodInstall(clinit)

          val oldLastBlock = m.code.blocks.last
          val lastBlock = m.code.newBlock
          oldLastBlock.replaceInstruction(oldLastBlock.length - 1, JUMP(lastBlock))

          if (isStaticModule(clasz.symbol)) {
            // call object's private ctor from static ctor
            lastBlock emit NEW(REFERENCE(m.symbol.enclClass))
            lastBlock emit CALL_METHOD(m.symbol.enclClass.primaryConstructor, Static(true))
          }

          // add serialVUID code
          serialVUID foreach { value =>
            import Flags._, definitions._
            val fieldName = "serialVersionUID"
            val fieldSymbol = clasz.symbol.newValue(NoPosition, newTermName(fieldName))
                                .setFlag(STATIC | FINAL)
                                .setInfo(longType)
            clasz addField new IField(fieldSymbol)
            lastBlock emit CONSTANT(Constant(value))
            lastBlock emit STORE_FIELD(fieldSymbol, true)
          }

          if (isParcelableClass)
            addCreatorCode(BytecodeGenerator.this, lastBlock)

          if (clasz.bootstrapClass.isDefined) {
            // emit bootstrap method install
            //emitBootstrapMethodInstall(block)
          }

          lastBlock emit RETURN(UNIT)
          lastBlock.close

       	  method = m
       	  jmethod = clinitMethod
       	  genCode(m)
       	case None =>
          legacyStaticInitializer(cls, clinit)
      }
    }

    private def legacyStaticInitializer(cls: JClass, clinit: JExtendedCode) {
      if (isStaticModule(clasz.symbol)) {
        clinit emitNEW cls.getName()
        clinit.emitINVOKESPECIAL(cls.getName(),
                                 JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                 JMethodType.ARGLESS_VOID_FUNCTION)
      }

      serialVUID foreach { value =>
        val fieldName = "serialVersionUID"
        jclass.addNewField(PublicStaticFinal, fieldName, JType.LONG)
        clinit emitPUSH value
        clinit.emitPUSH(value)
        clinit.emitPUTSTATIC(jclass.getName(), fieldName, JType.LONG)
      }

      if (isParcelableClass)
        legacyAddCreatorCode(BytecodeGenerator.this, clinit)

      if (clasz.bootstrapClass.isDefined)
        legacyEmitBootstrapMethodInstall(clinit)

      clinit.emitRETURN()
    }

    /** Emit code that installs a boostrap method for invoke dynamic. It
     *  installs the default method, found in scala.runtime.DynamicDispatch.
     */
    def legacyEmitBootstrapMethodInstall(jcode: JExtendedCode) {
      jcode emitPUSH jclass.getType.asInstanceOf[JReferenceType]
      jcode emitPUSH new JObjectType("scala.runtime.DynamicDispatch")
      jcode emitPUSH "bootstrapInvokeDynamic"
      jcode.emitGETSTATIC("java.dyn.Linkage", "BOOTSTRAP_METHOD_TYPE", MethodTypeType)
      jcode.emitDUP
      jcode.emitINVOKESTATIC("scala.Console", "println", new JMethodType(JType.VOID, Array(JAVA_LANG_OBJECT)))
      jcode.emitINVOKESTATIC("java.dyn.MethodHandles", "findStatic",
                              new JMethodType(MethodHandleType, Array(JavaLangClassType, JAVA_LANG_STRING, MethodTypeType)))
      jcode.emitINVOKESTATIC("java.dyn.Linkage", "registerBootstrapMethod",
                              new JMethodType(JType.VOID, Array(JavaLangClassType, MethodHandleType)))
    }

    /** Add a forwarder for method m */
    def addForwarder(jclass: JClass, module: Symbol, m: Symbol, accessFlags: Int) {
      val moduleName     = javaName(module)
      val mirrorName     = moduleName dropRight 1 // dropping '$'
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes = methodInfo.paramTypes map toTypeKind
      val paramNames     = 0 until paramJavaTypes.length map ("x_" + _) toArray

      val mirrorMethod = jclass.addNewMethod(
        accessFlags,
        javaName(m),
        javaType(methodInfo.resultType),
        javaTypes(paramJavaTypes),
        paramNames)
      val mirrorCode = mirrorMethod.getCode().asInstanceOf[JExtendedCode]
      mirrorCode.emitGETSTATIC(moduleName,
                               nme.MODULE_INSTANCE_FIELD.toString,
                               new JObjectType(moduleName))

      var i = 0
      var index = 0
      var argTypes = mirrorMethod.getArgumentTypes()
      while (i < argTypes.length) {
        mirrorCode.emitLOAD(index, argTypes(i))
        index += argTypes(i).getSize()
        i += 1
      }

      mirrorCode.emitINVOKEVIRTUAL(moduleName, mirrorMethod.getName, javaType(m).asInstanceOf[JMethodType])
      mirrorCode emitRETURN mirrorMethod.getReturnType()

      addRemoteException(mirrorMethod, m)
      // only add generic signature if the method is concrete; bug #1745
      if (!m.isDeferred)
        addGenericSignature(mirrorMethod, m, module)

      val (throws, others) = splitAnnotations(m.annotations, ThrowsClass)
      addExceptionsAttribute(mirrorMethod, throws)
      addAnnotations(mirrorMethod, others)
      addParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))
    }

    /** Add forwarders for all methods defined in `module' that don't conflict
     *  with methods in the companion class of `module'. A conflict arises when
     *  a method with the same name is defined both in a class and its companion
     *  object: method signature is not taken into account.
     */
    def addForwarders(jclass: JClass, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass)
      val className    = jclass.getName
      val linkedClass  = moduleClass.companionClass
      val linkedModule = linkedClass.companionSymbol

      /** If we use the usual algorithm for forwarders, we run into a problem if
       *  an object extends its companion class.  However, there is an out: since
       *  all the forwarders are static, inheriting from the class is no problem
       *  so long as the methods aren't final (the JVM will not allow redefinition
       *  of a final static method.) Thus the following.
       */
      val isIncestuous = moduleClass.tpe <:< linkedClass.tpe
      val accessFlags  = if (isIncestuous) PublicStatic else PublicStaticFinal

      /** There was a bit of a gordian logic knot here regarding forwarders.
       *  All we really have to do is exclude certain categories of symbols and
       *  then all matching names.
       */
      def memberNames(sym: Symbol) = sym.info.members map (_.name.toString) toSet
      lazy val membersInCommon     =
        memberNames(linkedModule) intersect memberNames(linkedClass)

      /** Should method `m' get a forwarder in the mirror class? */
      def shouldForward(m: Symbol): Boolean = (
        m.owner != ObjectClass
        && m.isMethod
        && m.isPublic
        && !m.hasFlag(Flags.CASE | Flags.DEFERRED | Flags.SPECIALIZED | Flags.LIFTED)
        && !m.isConstructor
        && !m.isStaticMember
        && !membersInCommon(m.name.toString)
      )

      for (m <- moduleClass.info.nonPrivateMembers) {
        if (shouldForward(m)) {
          log("Adding static forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
          addForwarder(jclass, moduleClass, m, accessFlags)
        }
        else if (settings.debug.value) {
          log("No forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
        }
      }
    }

    /** Dump a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     */
    def dumpMirrorClass(clasz: Symbol, sourceFile: String) {
      val moduleName = javaName(clasz) // + "$"
      val mirrorName = moduleName dropRight 1
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           JAVA_LANG_OBJECT.getName,
                                           JClass.NO_INTERFACES,
                                           sourceFile)

      log("Dumping mirror class for '%s'".format(mirrorClass.getName))
      addForwarders(mirrorClass, clasz)
      val ssa = scalaSignatureAddingMarker(mirrorClass, clasz.companionSymbol)
      addAnnotations(mirrorClass, clasz.annotations ++ ssa)
      emitClass(mirrorClass, clasz)
    }

    var linearization: List[BasicBlock] = Nil
    var isModuleInitialized = false

    /**
     *  @param m ...
     */
    def genCode(m: IMethod) {
      val jcode = jmethod.getCode.asInstanceOf[JExtendedCode]

      def makeLabels(bs: List[BasicBlock]) = {
        if (settings.debug.value)
          log("Making labels for: " + method)

        mutable.HashMap(bs map (_ -> jcode.newLabel) : _*)
      }

      isModuleInitialized = false

      linearization = linearizer.linearize(m)
      val labels = makeLabels(linearization)
      /** local variables whose scope appears in this block. */
      val varsInBlock: mutable.Set[Local] = new mutable.HashSet

      var nextBlock: BasicBlock = linearization.head

      def genBlocks(l: List[BasicBlock]): Unit = l match {
        case Nil => ()
        case x :: Nil => nextBlock = null; genBlock(x)
        case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys)
      }

    /** Generate exception handlers for the current method. */
    def genExceptionHandlers {

      /** Return a list of pairs of intervals where the handler is active.
       *  The intervals in the list have to be inclusive in the beginning and
       *  exclusive in the end: [start, end).
       */
      def ranges(e: ExceptionHandler): List[(Int, Int)] = {
        var covered = e.covered
        var ranges: List[(Int, Int)] = Nil
        var start = -1
        var end = -1

        linearization foreach { b =>
          if (! (covered contains b) ) {
            if (start >= 0) { // we're inside a handler range
              end = labels(b).getAnchor()
              ranges ::= (start, end)
              start = -1
            }
          } else {
            if (start < 0)  // we're not inside a handler range
              start = labels(b).getAnchor()

            end = endPC(b)
            covered -= b
          }
        }

        /* Add the last interval. Note that since the intervals are
         * open-ended to the right, we have to give a number past the actual
         * code!
         */
        if (start >= 0) {
          ranges ::= (start, jcode.getPC())
        }

        if (!covered.isEmpty)
          if (settings.debug.value)
            log("Some covered blocks were not found in method: " + method +
                " covered: " + covered + " not in " + linearization)
        ranges
      }

      for (e <- this.method.exh ; p <- ranges(e).sortBy(_._1)) {
        if (p._1 < p._2) {
          if (settings.debug.value)
            log("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
                " from: " + p._1 + " to: " + p._2 + " catching: " + e.cls);
          val cls = if (e.cls == NoSymbol || e.cls == ThrowableClass) null
                    else javaName(e.cls)
          jcode.addExceptionHandler(p._1, p._2,
                                    labels(e.startBlock).getAnchor(),
                                    cls)
        } else
          log("Empty exception range: " + p)
      }
    }

    def genBlock(b: BasicBlock) {
      labels(b).anchorToNext()

      if (settings.debug.value)
        log("Generating code for block: " + b + " at pc: " + labels(b).getAnchor())
      var lastMappedPC = 0
      var lastLineNr = 0
      var crtPC = 0
      varsInBlock.clear()

      for (instr <- b) {
        class CompilationException(msg: String) extends Exception(msg) {
          override def toString: String = {
            msg +
            "\nCurrent method: " + method +
            "\nCurrent block: " + b +
            "\nCurrent instruction: " + instr +
            "\n---------------------" +
            method.dump
          }
        }
        def assert(cond: Boolean, msg: String) =
          if (!cond) throw new CompilationException(msg)

        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0()

          case CONSTANT(const) =>
            genConstant(jcode, const)

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind))

          case LOAD_LOCAL(local) =>
            jcode.emitLOAD(indexOf(local), javaType(local.kind))

          case LOAD_FIELD(field, isStatic) =>
            var owner = javaName(field.owner)
            if (settings.debug.value)
              log("LOAD_FIELD with owner: " + owner +
                  " flags: " + Flags.flagsToString(field.owner.flags))
            if (isStatic)
              jcode.emitGETSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitGETFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case LOAD_MODULE(module) =>
//            assert(module.isModule, "Expected module: " + module)
            if (settings.debug.value)
              log("generating LOAD_MODULE for: " + module + " flags: " +
                  Flags.flagsToString(module.flags));
            if (clasz.symbol == module.moduleClass && jmethod.getName() != nme.readResolve.toString)
              jcode.emitALOAD_0()
            else
              jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                  nme.MODULE_INSTANCE_FIELD.toString,
                                  javaType(module));

          case STORE_ARRAY_ITEM(kind) =>
            jcode emitASTORE javaType(kind)

          case STORE_LOCAL(local) =>
            jcode.emitSTORE(indexOf(local), javaType(local.kind))

          case STORE_THIS(_) =>
            // this only works for impl classes because the self parameter comes first
            // in the method signature. If that changes, this code has to be revisited.
            jcode.emitASTORE_0()

          case STORE_FIELD(field, isStatic) =>
            val owner = javaName(field.owner)
            if (isStatic)
              jcode.emitPUTSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitPUTFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)

          /** Special handling to access native Array.clone() */
          case call @ CALL_METHOD(definitions.Array_clone, Dynamic) =>
            val target: String = javaType(call.targetTypeKind).getSignature()
            jcode.emitINVOKEVIRTUAL(target, "clone", arrayCloneType)

          case call @ CALL_METHOD(method, style) =>
            val owner: String = javaName(method.owner)
            // reference the type of the receiver instead of the method owner (if not an interface!)
            val dynamicOwner =
              if (needsInterfaceCall(call.hostClass)) owner
              else javaName(call.hostClass)
            val jname = javaName(method)
            val jtype = javaType(method).asInstanceOf[JMethodType]

            style match {
              case InvokeDynamic =>
                jcode.emitINVOKEINTERFACE("java.dyn.Dynamic", jname, jtype)

              case Dynamic =>
                if (needsInterfaceCall(method.owner))
                  jcode.emitINVOKEINTERFACE(owner, jname, jtype)
                else
                  jcode.emitINVOKEVIRTUAL(dynamicOwner, jname, jtype)

              case Static(instance) =>
                if (instance)
                  jcode.emitINVOKESPECIAL(owner, jname, jtype)
                else
                  jcode.emitINVOKESTATIC(owner, jname, jtype)

              case SuperCall(_) =>
                jcode.emitINVOKESPECIAL(owner, jname, jtype)
                // we initialize the MODULE$ field immediately after the super ctor
                if (isStaticModule(clasz.symbol) && !isModuleInitialized &&
                    jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
                    jname == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
                  isModuleInitialized = true
                  jcode.emitALOAD_0()
                  jcode.emitPUTSTATIC(jclass.getName(),
                                      nme.MODULE_INSTANCE_FIELD.toString,
                                      jclass.getType())
                }
            }

          case BOX(kind) =>
            val boxedType = definitions.boxedClass(kind.toType.typeSymbol)
            val mtype = new JMethodType(javaType(boxedType), Array(javaType(kind)))
            jcode.emitINVOKESTATIC(BoxesRunTime, "boxTo" + boxedType.decodedName, mtype)

          case UNBOX(kind) =>
            val mtype = new JMethodType(javaType(kind), Array(JAVA_LANG_OBJECT))
            jcode.emitINVOKESTATIC(BoxesRunTime, "unboxTo" + kind.toType.typeSymbol.decodedName, mtype)

          case NEW(REFERENCE(cls)) =>
            val className = javaName(cls)
            jcode emitNEW className

          case CREATE_ARRAY(elem, 1) => elem match {
            case REFERENCE(_) | ARRAY(_) =>
              jcode emitANEWARRAY javaType(elem).asInstanceOf[JReferenceType]
            case _ =>
              jcode emitNEWARRAY javaType(elem)
          }

          case CREATE_ARRAY(elem, dims) =>
            jcode.emitMULTIANEWARRAY(javaType(ArrayN(elem, dims)).asInstanceOf[JReferenceType], dims)

          case IS_INSTANCE(tpe) =>
            tpe match {
              case REFERENCE(cls) =>
                jcode emitINSTANCEOF new JObjectType(javaName(cls))
              case ARRAY(elem) =>
                jcode emitINSTANCEOF new JArrayType(javaType(elem))
              case _ =>
                abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case CHECK_CAST(tpe) =>
            tpe match {
              case REFERENCE(cls) =>
                jcode emitCHECKCAST new JObjectType(javaName(cls))
              case ARRAY(elem) =>
                jcode emitCHECKCAST new JArrayType(javaType(elem))
              case _ =>
                abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case SWITCH(tags, branches) =>
            val tagArray = new Array[Array[Int]](tags.length)
            var caze = tags
            var i = 0

            while (i < tagArray.length) {
              tagArray(i) = new Array[Int](caze.head.length)
              caze.head.copyToArray(tagArray(i), 0)
              i += 1
              caze = caze.tail
            }
            val branchArray = jcode.newLabels(tagArray.length)
            i = 0
            while (i < branchArray.length) {
              branchArray(i) = labels(branches(i))
              i += 1
            }
            if (settings.debug.value)
              log("Emitting SWITCH:\ntags: " + tags + "\nbranches: " + branches)
            jcode.emitSWITCH(tagArray,
                             branchArray,
                             labels(branches.last),
                             MIN_SWITCH_DENSITY)
            ()

          case JUMP(whereto) =>
            if (nextBlock != whereto)
              jcode.emitGOTO_maybe_W(labels(whereto), false); // default to short jumps

          case CJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   => jcode.emitLCMP()
                  case FLOAT  =>
                    if (cond == LT || cond == LE) jcode.emitFCMPG()
                    else jcode.emitFCMPL()
                  case DOUBLE =>
                    if (cond == LT || cond == LE) jcode.emitDCMPG()
                    else jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case REFERENCE(_) | ARRAY(_) =>
                val Success = success
                val Failure = failure
                (cond, nextBlock) match {
                  case (EQ, Success) =>
                    jcode emitIFNONNULL labels(failure)
                  case (NE, Failure) =>
                    jcode emitIFNONNULL labels(success)
                  case (EQ, Failure) =>
                    jcode emitIFNULL labels(success)
                  case (NE, Success) =>
                    jcode emitIFNULL labels(failure)
                  case (EQ, _) =>
                    jcode emitIFNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                  case (NE, _) =>
                    jcode emitIFNONNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   =>
                    jcode.emitLCONST_0(); jcode.emitLCMP()
                  case FLOAT  =>
                    jcode.emitFCONST_0()
                    if (cond == LT || cond == LE) jcode.emitFCMPG()
                    else jcode.emitFCMPL()
                  case DOUBLE =>
                    jcode.emitDCONST_0()
                    if (cond == LT || cond == LE) jcode.emitDCMPG()
                    else jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
            }

          case RETURN(kind) =>
            jcode emitRETURN javaType(kind)

          case THROW(_) =>
            jcode.emitATHROW()

          case DROP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitPOP2()
              case _ => jcode.emitPOP()
            }

          case DUP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitDUP2()
              case _ => jcode.emitDUP()
            }

          case MONITOR_ENTER() =>
            jcode.emitMONITORENTER()

          case MONITOR_EXIT() =>
            jcode.emitMONITOREXIT()

          case SCOPE_ENTER(lv) =>
            varsInBlock += lv
            lv.start = jcode.getPC()

          case SCOPE_EXIT(lv) =>
            if (varsInBlock(lv)) {
              lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
              varsInBlock -= lv
            }
            else if (b.varsInScope(lv)) {
              lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
              b.varsInScope -= lv
            } else
              assert(false, "Illegal local var nesting: " + method)

          case LOAD_EXCEPTION(_) =>
            ()
        }

        crtPC = jcode.getPC()

//        assert(instr.pos.source.isEmpty || instr.pos.source.get == (clasz.cunit.source), "sources don't match")
//        val crtLine = instr.pos.line.get(lastLineNr);
        val crtLine = try {
          (instr.pos).line
        } catch {
          case _: UnsupportedOperationException =>
            log("Warning: wrong position in: " + method)
            lastLineNr
        }

        if (b.lastInstruction == instr)
          endPC(b) = jcode.getPC()

        //System.err.println("CRTLINE: " + instr.pos + " " +
        //           /* (if (instr.pos < clasz.cunit.source.content.length) clasz.cunit.source.content(instr.pos) else '*') + */ " " + crtLine);

        if (crtPC > lastMappedPC) {
          jcode.completeLineNumber(lastMappedPC, crtPC, crtLine)
          lastMappedPC = crtPC
          lastLineNr   = crtLine
        }
      }

      // local vars that survived this basic block
      for (lv <- varsInBlock) {
        lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
      }
      for (lv <- b.varsInScope) {
        lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
      }
    }


    /**
     *  @param primitive ...
     *  @param pos       ...
     */
    def genPrimitive(primitive: Primitive, pos: Position) {
      primitive match {
        case Negation(kind) =>
          kind match {
            case BOOL | BYTE | CHAR | SHORT | INT =>
              jcode.emitINEG()
            case LONG   => jcode.emitLNEG()
            case FLOAT  => jcode.emitFNEG()
            case DOUBLE => jcode.emitDNEG()
            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => jcode.emitADD(javaType(kind))
            case SUB =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitISUB()
                case LONG   => jcode.emitLSUB()
                case FLOAT  => jcode.emitFSUB()
                case DOUBLE => jcode.emitDSUB()
              }

            case MUL =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIMUL()
                case LONG   => jcode.emitLMUL()
                case FLOAT  => jcode.emitFMUL()
                case DOUBLE => jcode.emitDMUL()
              }

            case DIV =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIDIV()
                case LONG   => jcode.emitLDIV()
                case FLOAT  => jcode.emitFDIV()
                case DOUBLE => jcode.emitDDIV()
              }

            case REM =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIREM()
                case LONG   => jcode.emitLREM()
                case FLOAT  => jcode.emitFREM()
                case DOUBLE => jcode.emitDREM()
              }

            case NOT =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitPUSH(-1)
                  jcode.emitIXOR()
                case LONG   =>
                  jcode.emitPUSH(-1l)
                  jcode.emitLXOR()
                case _ =>
                  abort("Impossible to negate an " + kind)
              }

            case _ =>
              abort("Unknown arithmetic primitive " + primitive)
          }

        case Logical(op, kind) => (op, kind) match {
          case (AND, LONG) =>
            jcode.emitLAND()
          case (AND, INT) =>
            jcode.emitIAND()
          case (AND, _) =>
            jcode.emitIAND()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (OR, LONG) =>
            jcode.emitLOR()
          case (OR, INT) =>
            jcode.emitIOR()
          case (OR, _) =>
            jcode.emitIOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (XOR, LONG) =>
            jcode.emitLXOR()
          case (XOR, INT) =>
            jcode.emitIXOR()
          case (XOR, _) =>
            jcode.emitIXOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));
        }

        case Shift(op, kind) => (op, kind) match {
          case (LSL, LONG) =>
            jcode.emitLSHL()
          case (LSL, INT) =>
            jcode.emitISHL()
          case (LSL, _) =>
            jcode.emitISHL()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (ASR, LONG) =>
            jcode.emitLSHR()
          case (ASR, INT) =>
            jcode.emitISHR()
          case (ASR, _) =>
            jcode.emitISHR()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (LSR, LONG) =>
            jcode.emitLUSHR()
          case (LSR, INT) =>
            jcode.emitIUSHR()
          case (LSR, _) =>
            jcode.emitIUSHR()
            jcode.emitT2T(javaType(INT), javaType(kind))
        }

        case Comparison(op, kind) => ((op, kind): @unchecked) match {
          case (CMP, LONG)    => jcode.emitLCMP()
          case (CMPL, FLOAT)  => jcode.emitFCMPL()
          case (CMPG, FLOAT)  => jcode.emitFCMPG()
          case (CMPL, DOUBLE) => jcode.emitDCMPL()
          case (CMPG, DOUBLE) => jcode.emitDCMPL()
        }

        case Conversion(src, dst) =>
          if (settings.debug.value)
            log("Converting from: " + src + " to: " + dst);
          if (dst == BOOL) {
            Console.println("Illegal conversion at: " + clasz +
                            " at: " + pos.source + ":" + pos.line);
          } else
            jcode.emitT2T(javaType(src), javaType(dst));

        case ArrayLength(_) =>
          jcode.emitARRAYLENGTH()

        case StartConcat =>
          jcode emitNEW StringBuilderClassName
          jcode.emitDUP()
          jcode.emitINVOKESPECIAL(StringBuilderClassName,
                                  JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                  JMethodType.ARGLESS_VOID_FUNCTION)

        case StringConcat(el) =>
          val jtype = el match {
            case REFERENCE(_) | ARRAY(_) => JAVA_LANG_OBJECT
            case _ => javaType(el)
          }
          jcode.emitINVOKEVIRTUAL(StringBuilderClassName,
                                  "append",
                                  new JMethodType(StringBuilderType,
                                  Array(jtype)))
        case EndConcat =>
          jcode.emitINVOKEVIRTUAL(StringBuilderClassName,
                                  "toString",
                                  toStringType)

        case _ =>
          abort("Unimplemented primitive " + primitive)
      }
    }

      // genCode starts here
      genBlocks(linearization)

      if (this.method.exh != Nil)
        genExceptionHandlers;
    }


    /** Emit a Local variable table for debugging purposes.
     *  Synthetic locals are skipped. All variables are method-scoped.
     */
    private def genLocalVariableTable(m: IMethod, jcode: JCode) {
      val vars = m.locals filterNot (_.sym.isSynthetic)
      if (vars.isEmpty) return

      val pool = jclass.getConstantPool
      val pc = jcode.getPC()
      var anonCounter = 0
      var entries = 0
      vars.foreach { lv =>
        lv.ranges = mergeEntries(lv.ranges.reverse);
        entries += lv.ranges.length
      }
      if (!jmethod.isStatic()) entries += 1

      val lvTab = ByteBuffer.allocate(2 + 10 * entries)
      def emitEntry(name: String, signature: String, idx: Short, start: Short, end: Short) {
        lvTab.putShort(start)
        lvTab.putShort(end)
        lvTab.putShort(pool.addUtf8(name).toShort)
        lvTab.putShort(pool.addUtf8(signature).toShort)
        lvTab.putShort(idx)
      }

      lvTab.putShort(entries.toShort)

      if (!jmethod.isStatic()) {
        emitEntry("this", jclass.getType().getSignature(), 0, 0.toShort, pc.toShort)
      }

      for (lv <- vars) {
        val name = if (javaName(lv.sym) eq null) {
          anonCounter += 1
          "<anon" + anonCounter + ">"
        } else javaName(lv.sym)

        val index = indexOf(lv).toShort
        val tpe   = javaType(lv.kind).getSignature()
        for ((start, end) <- lv.ranges) {
          emitEntry(name, tpe, index, start.toShort, (end - start).toShort)
        }
      }
      val attr =
        fjbgContext.JOtherAttribute(jclass,
                                    jmethod,
                                    tpnme.LocalVariableTableATTR.toString,
                                    lvTab.array())
      jcode addAttribute attr
    }


    /** For each basic block, the first PC address following it. */
    val endPC = new mutable.HashMap[BasicBlock, Int]

    ////////////////////// local vars ///////////////////////

    def sizeOf(sym: Symbol): Int = sizeOf(toTypeKind(sym.tpe))

    def sizeOf(k: TypeKind): Int = k match {
      case DOUBLE | LONG => 2
      case _ => 1
    }

    def indexOf(m: IMethod, sym: Symbol): Int = {
      val Some(local) = m.lookupLocal(sym)
      indexOf(local)
    }

    def indexOf(local: Local): Int = {
      assert(local.index >= 0,
             "Invalid index for: " + local + "{" + local.## + "}: ")
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given
     * method. Assumes parameters come first in the list of locals.
     */
    def computeLocalVarsIndex(m: IMethod) {
      var idx = 1
      if (m.symbol.isStaticMember)
        idx = 0;

      for (l <- m.locals) {
        if (settings.debug.value)
          log("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }
    }

    ////////////////////// Utilities ////////////////////////

    override def javaName(sym: Symbol): String = {
      if (sym.isClass && !sym.rawowner.isPackageClass && !sym.isModuleClass)
        innerClasses = innerClasses + sym
      super.javaName(sym)
    }

    /** Calls to methods in 'sym' need invokeinterface? */
    def needsInterfaceCall(sym: Symbol): Boolean = {
      log("checking for interface call: " + sym.fullName)
      // the following call to 'info' may cause certain symbols to fail loading
      // because we're too late in the compilation chain (aliases to overloaded
      // symbols will not be properly resolved, see scala.Range, method
      // `super$++` that fails in UnPickler at LazyTypeRefAndAlias.complete
      if (sym.isTrait) sym.info // needed so that the type is up to date
                                // (erasure may add lateINTERFACE to traits)

      sym.isInterface ||
      (sym.isJavaDefined && sym.isNonBottomSubClass(ClassfileAnnotationClass))
    }

    /** Return an abstract file for the given class symbol, with the desired suffix.
     *  Create all necessary subdirectories on the way.
     */
    def getFile(sym: Symbol, cls: JClass, suffix: String): AbstractFile = {
      val sourceFile = atPhase(currentRun.flattenPhase.prev)(sym.sourceFile)
      var dir: AbstractFile = settings.outputDirs.outputDirFor(sourceFile)
      val pathParts = cls.getName().split("[./]").toList
      for (part <- pathParts.init) {
        dir = dir.subdirectoryNamed(part)
      }
      dir.fileNamed(pathParts.last + suffix)
    }

    /** Merge adjacent ranges. */
    private def mergeEntries(ranges: List[(Int, Int)]): List[(Int, Int)] =
      (ranges.foldLeft(Nil: List[(Int, Int)]) { (collapsed: List[(Int, Int)], p: (Int, Int)) => (collapsed, p) match {
        case (Nil, _) => List(p)
        case ((s1, e1) :: rest, (s2, e2)) if (e1 == s2) => (s1, e2) :: rest
        case _ => p :: collapsed
      }}).reverse

    def assert(cond: Boolean, msg: => String) = if (!cond) {
      method.dump
      abort(msg + "\nMethod: " + method)
    }

    def assert(cond: Boolean) { assert(cond, "Assertion failed.") }
  }

  /**
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final, strictfp (not used)
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   *
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   */
  def javaFlags(sym: Symbol): Int = {
    def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)
    // constructors of module classes should be private
    // PP: why are they only being marked private at this stage and not earlier?
    val isConsideredPrivate =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModule(sym.owner))

    mkFlags(
      if (isConsideredPrivate) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (sym.isFinal && !sym.enclClass.isInterface && !sym.isClassConstructor) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge) ACC_BRIDGE else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0
    )
  }

  def isTopLevelModule(sym: Symbol): Boolean =
    atPhase (currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

}
