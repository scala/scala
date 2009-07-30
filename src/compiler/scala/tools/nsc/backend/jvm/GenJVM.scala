/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc
package backend.jvm

import java.io.{DataOutputStream, File, OutputStream}
import java.nio.ByteBuffer

import scala.collection.immutable.{Set, ListSet}
import scala.collection.mutable.{Map, HashMap, HashSet}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.{Position, NoPosition}
import scala.tools.nsc.symtab.classfile.ClassfileConstants._

import ch.epfl.lamp.fjbg._

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 *
 */
abstract class GenJVM extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase) = new JvmPhase(p)

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    override def erasedTypes = true
    object codeGenerator extends BytecodeGenerator

    override def run {
      if (settings.debug.value) inform("[running phase " + name + " on icode]")
      if (settings.Xdce.value)
        icodes.classes.retain { (sym: Symbol, cls: IClass) => !inliner.isClosureClass(sym) || deadCode.liveClosures(sym) }

      classes.values foreach apply
    }

    override def apply(cls: IClass) {
      codeGenerator.genClass(cls)
    }
  }

  var pickledBytes = 0 // statistics

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator {
    val MIN_SWITCH_DENSITY = 0.7
    val StringBuilderClass = definitions.getClass2("scala.StringBuilder", "scala.collection.mutable.StringBuilder").fullNameString
    val BoxesRunTime = "scala.runtime.BoxesRunTime"

    val StringBuilderType = new JObjectType(StringBuilderClass)
    val toStringType      = new JMethodType(JObjectType.JAVA_LANG_STRING, JType.EMPTY_ARRAY)
    val MethodTypeType    = new JObjectType("java.dyn.MethodType")
    val JavaLangClassType = new JObjectType("java.lang.Class")
    val MethodHandleType  = new JObjectType("java.dyn.MethodHandle")

    // Scala attributes
    val SerializableAttr = definitions.SerializableAttr
    val SerialVersionUID = definitions.getClass("scala.SerialVersionUID")
    val CloneableAttr    = definitions.getClass("scala.cloneable")
    val TransientAtt     = definitions.getClass("scala.transient")
    val VolatileAttr     = definitions.getClass("scala.volatile")
    val RemoteAttr       = definitions.getClass("scala.remote")
    val ThrowsAttr       = definitions.getClass("scala.throws")
    val BeanInfoAttr     = definitions.getClass("scala.reflect.BeanInfo")
    val BeanInfoSkipAttr = definitions.getClass("scala.reflect.BeanInfoSkip")
    val BeanDisplayNameAttr = definitions.getClass("scala.reflect.BeanDisplayName")
    val BeanDescriptionAttr = definitions.getClass("scala.reflect.BeanDescription")

    lazy val CloneableClass  = definitions.getClass("java.lang.Cloneable")
    lazy val RemoteInterface = definitions.getClass("java.rmi.Remote")
    lazy val RemoteException = definitions.getClass("java.rmi.RemoteException").tpe

    var clasz: IClass = _
    var method: IMethod = _
    var jclass: JClass = _
    var jmethod: JMethod = _
//    var jcode: JExtendedCode = _

    var innerClasses: Set[Symbol] = ListSet.empty // referenced inner classes

    val fjbgContext = new FJBGContext(49, 0)

    val emitSource = settings.debuginfo.level >= 1
    val emitLines  = settings.debuginfo.level >= 2
    val emitVars   = settings.debuginfo.level >= 3

    /** Write a class to disk, adding the Scala signature (pickled type information) and
     *  inner classes.
     *
     * @param jclass The FJBG class, where code was emitted
     * @param sym    The corresponding symbol, used for looking up pickled information
     */
    def emitClass(jclass: JClass, sym: Symbol) {
      def addScalaAttr(sym: Symbol): Unit = currentRun.symData.get(sym) match {
        case Some(pickle) =>
          val scalaAttr = fjbgContext.JOtherAttribute(jclass,
                                                  jclass,
                                                  nme.ScalaSignatureATTR.toString,
                                                  pickle.bytes,
                                                  pickle.writeIndex)
          pickledBytes = pickledBytes + pickle.writeIndex
          jclass.addAttribute(scalaAttr)
          currentRun.symData -= sym
          currentRun.symData -= sym.linkedSym
          //System.out.println("Generated ScalaSig Attr for " + sym)//debug
        case _ =>
          val markerAttr = getMarkerAttr(jclass)
          jclass.addAttribute(markerAttr)
          log("Could not find pickle information for " + sym)
      }
      if (!(jclass.getName().endsWith("$") && sym.isModuleClass))
        addScalaAttr(if (isTopLevelModule(sym)) sym.sourceModule else sym);
      addInnerClasses(jclass)

      val outfile = getFile(sym, jclass, ".class")
      val outstream = new DataOutputStream(outfile.output)
      jclass.writeTo(outstream)
      outstream.close()
      informProgress("wrote " + outfile)
    }

    private def getMarkerAttr(jclass: JClass): JOtherAttribute =
      fjbgContext.JOtherAttribute(jclass, jclass, nme.ScalaATTR.toString, new Array[Byte](0), 0)

    var serialVUID: Option[Long] = None
    var remoteClass: Boolean = false

    def genClass(c: IClass) {
      clasz = c
      innerClasses = ListSet.empty

      var parents = c.symbol.info.parents
      var ifaces  = JClass.NO_INTERFACES
      val name    = javaName(c.symbol)
      serialVUID  = None
      remoteClass = false

      if (parents.isEmpty)
        parents = definitions.ObjectClass.tpe :: parents;

      for (annot <- c.symbol.annotations) annot match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == SerializableAttr =>
          parents = parents ::: List(definitions.SerializableClass.tpe)
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == CloneableAttr =>
          parents = parents ::: List(CloneableClass.tpe)
        case AnnotationInfo(tp, Literal(const) :: _, _) if tp.typeSymbol == SerialVersionUID =>
          serialVUID = Some(const.longValue)
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == RemoteAttr =>
          parents = parents ::: List(RemoteInterface.tpe)
          remoteClass = true
        case _ => ()
      }

      parents = parents.removeDuplicates

      if (parents.length > 1) {
        ifaces = new Array[String](parents.length - 1)
        parents.drop(1).map((s) => javaName(s.typeSymbol)).copyToArray(ifaces, 0)
        ()
      }

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(parents(0).typeSymbol),
                                  ifaces,
                                  c.cunit.source.toString)
      if (jclass.getName.endsWith("$"))
        jclass.addAttribute(getMarkerAttr(jclass))

      if (isStaticModule(c.symbol) || serialVUID != None || clasz.bootstrapClass.isDefined) {
        if (isStaticModule(c.symbol))
            addModuleInstanceField;
        addStaticInit(jclass)

        if (isTopLevelModule(c.symbol)) {
          if (c.symbol.linkedClassOfModule == NoSymbol)
            dumpMirrorClass(c.symbol, c.cunit.source.toString);
          else
            log("No mirror class for module with linked class: " +
                c.symbol.fullNameString)
        }
      }
      else {
        val lmoc = c.symbol.linkedModuleOfClass
        // add static forwarders if there are no name conflicts; see bugs #363 and #1735
        if (lmoc != NoSymbol && !c.symbol.hasFlag(Flags.INTERFACE)) {
          // forwarders were creating many issues, see #1795 for instance; so for now
          // they are only enabled if -Xforwarders is supplied to scalac...
          if (settings.forwarders.value) {
            log("Adding forwarders to existing class " + c.symbol + " found in module " + lmoc)
            addForwarders(jclass, lmoc.moduleClass)
          }
          // ...but we special case main so at least ticket #363 can continue to work.
          // XXX we would like to, but doing so induces #1795 as surely as adding them all.
          // else addForwarders(jclass, lmoc.moduleClass, _.name == nme.main)
        }
      }

      if (clasz.bootstrapClass.isDefined) jclass.setBootstrapClass(clasz.bootstrapClass.get)
      clasz.fields foreach genField
      clasz.methods foreach genMethod

      addGenericSignature(jclass, c.symbol, c.symbol.owner)
      addAnnotations(jclass, c.symbol.annotations)
      emitClass(jclass, c.symbol)

      if (c.symbol hasAnnotation BeanInfoAttr)
        genBeanInfoClass(c)
    }


    /**
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     */
    def genBeanInfoClass(c: IClass) {
      val description = c.symbol.annotations.find(_.atp.typeSymbol == BeanDescriptionAttr)
      // informProgress(description.toString())

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

      val constructor = beanInfoClass.addNewMethod(JAccessFlags.ACC_PUBLIC, "<init>", JType.VOID, javaTypes(Nil), javaNames(Nil))
      val jcode = constructor.getCode().asInstanceOf[JExtendedCode]
      val strKind = new JObjectType(javaName(definitions.StringClass))
      val stringArrayKind = new JArrayType(strKind)
      val conType = new JMethodType(JType.VOID, Array(javaType(definitions.ClassClass), stringArrayKind, stringArrayKind))

      def push(lst:Seq[String]) {
        var fi = 0
        for (f <- lst) {
          jcode.emitDUP()
          jcode.emitPUSH(fi)
          if (f != null)
            jcode.emitPUSH(f)
          else
            jcode.emitACONST_NULL()
          jcode.emitASTORE(strKind)
          fi += 1
        }
      }

      jcode.emitALOAD_0()
      // push the class
      jcode.emitPUSH(javaType(c.symbol).asInstanceOf[JReferenceType])

      // push the the string array of field information
      jcode.emitPUSH(fieldList.length)
      jcode.emitANEWARRAY(strKind)
      push(fieldList)

      // push the string array of method information
      jcode.emitPUSH(methodList.length)
      jcode.emitANEWARRAY(strKind)
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      jcode.emitINVOKESPECIAL("scala/reflect/ScalaBeanInfo", "<init>", conType)
      jcode.emitRETURN()

      // write the bean information class file.
      val outfile = getFile(c.symbol, beanInfoClass, ".class")
      val outstream = new DataOutputStream(outfile.output)
      beanInfoClass.writeTo(outstream)
      outstream.close()
      informProgress("wrote BeanInfo " + outfile)
    }


    /** Add the given 'throws' attributes to jmethod */
    def addExceptionsAttribute(jmethod: JMethod, excs: List[AnnotationInfo]) {
      if (excs.isEmpty) return

      val cpool = jmethod.getConstantPool()
      val buf: ByteBuffer = ByteBuffer.allocate(512)
      var nattr = 0

      // put some radom value; the actual number is determined at the end
      buf.putShort(0xbaba.toShort)

      for (AnnotationInfo(tp, List(exc), _) <- excs.removeDuplicates if tp.typeSymbol == ThrowsAttr) {
        val Literal(const) = exc
        buf.putShort(
          cpool.addClass(
            javaName(const.typeValue.typeSymbol)).shortValue)
        nattr += 1
      }

      assert(nattr > 0)
      buf.putShort(0, nattr.toShort)
      addAttribute(jmethod, nme.ExceptionsATTR, buf)
    }

    /** Whether an annotation should be emitted as a Java annotation
     *   .initialize: if 'annnot' is read from pickle, atp might be un-initialized
     */
    private def shouldEmitAnnotation(annot: AnnotationInfo) =
      (annot.atp.typeSymbol.initialize.hasFlag(Flags.JAVA) &&
       annot.atp.typeSymbol.isNonBottomSubClass(definitions.ClassfileAnnotationClass) &&
       annot.args.isEmpty)

    private def emitJavaAnnotations(cpool: JConstantPool, buf: ByteBuffer, annotations: List[AnnotationInfo]): Int = {
      def emitArgument(arg: ClassfileAnnotArg): Unit = arg match {
        case LiteralAnnotArg(const) =>
          const.tag match {
            case BooleanTag =>
              buf.put('Z'.toByte)
              buf.putShort(cpool.addInteger(if(const.booleanValue) 1 else 0).toShort)
            case ByteTag    =>
              buf.put('B'.toByte)
              buf.putShort(cpool.addInteger(const.byteValue).toShort)
            case ShortTag   =>
              buf.put('S'.toByte)
              buf.putShort(cpool.addInteger(const.shortValue).toShort)
            case CharTag    =>
              buf.put('C'.toByte)
              buf.putShort(cpool.addInteger(const.charValue).toShort)
            case IntTag     =>
              buf.put('I'.toByte)
              buf.putShort(cpool.addInteger(const.intValue).toShort)
            case LongTag    =>
              buf.put('J'.toByte)
              buf.putShort(cpool.addLong(const.longValue).toShort)
            case FloatTag   =>
              buf.put('F'.toByte)
              buf.putShort(cpool.addFloat(const.floatValue).toShort)
            case DoubleTag  =>
              buf.put('D'.toByte)
              buf.putShort(cpool.addDouble(const.doubleValue).toShort)
            case StringTag  =>
              buf.put('s'.toByte)
              buf.putShort(cpool.addUtf8(const.stringValue).toShort)
            case ClassTag   =>
              buf.put('c'.toByte)
              buf.putShort(cpool.addUtf8(javaType(const.typeValue).getSignature()).toShort)
            case EnumTag =>
              buf.put('e'.toByte)
              buf.putShort(cpool.addUtf8(javaType(const.tpe).getSignature()).toShort)
              buf.putShort(cpool.addUtf8(const.symbolValue.name.toString).toShort)
          }

        case ArrayAnnotArg(args) =>
          buf.put('['.toByte)
          buf.putShort(args.length.toShort)
          for (val elem <- args) emitArgument(elem)

        case NestedAnnotArg(annInfo) =>
          buf.put('@'.toByte)
          emitAnnotation(annInfo)
      }

      def emitAnnotation(annotInfo: AnnotationInfo) {
        val AnnotationInfo(typ, args, assocs) = annotInfo
        val jtype = javaType(typ)
        buf.putShort(cpool.addUtf8(jtype.getSignature()).toShort)
        assert(args.isEmpty, args.toString)
        buf.putShort(assocs.length.toShort)
        for ((name, value) <- assocs) {
          buf.putShort(cpool.addUtf8(name.toString).toShort)
          emitArgument(value)
        }
      }

      var nannots = 0
      val pos = buf.position()

      // put some random value; the actual number of annotations is determined at the end
      buf.putShort(0xbaba.toShort)

      for (annot <- annotations if shouldEmitAnnotation(annot)) {
        nannots += 1
        emitAnnotation(annot)
      }

      // save the number of annotations
      buf.putShort(pos, nannots.toShort)
      nannots
    }

    def addGenericSignature(jmember: JMember, sym: Symbol, owner: Symbol) {
      if (!sym.hasFlag(Flags.EXPANDEDNAME | Flags.SYNTHETIC)
          && !(sym.isMethod && sym.hasFlag(Flags.LIFTED))) {
        val memberTpe = atPhase(currentRun.erasurePhase)(owner.thisType.memberInfo(sym))
//        println("sym: " + sym.fullNameString + " : " + memberTpe + " sym.info: " + sym.info)
        erasure.javaSig(sym, memberTpe) match {
          case Some(sig) =>
            val index = jmember.getConstantPool().addUtf8(sig).toShort
            if (settings.debug.value && settings.verbose.value)
              atPhase(currentRun.erasurePhase) {
                println("add generic sig "+sym+":"+sym.info+" ==> "+sig+" @ "+index)
              }
            val buf = ByteBuffer.allocate(2)
            buf.putShort(index)
            addAttribute(jmember, nme.SignatureATTR, buf)
          case None =>
        }
      }
    }

    def addAnnotations(jmember: JMember, annotations: List[AnnotationInfo]) {
      val toEmit = annotations.filter(shouldEmitAnnotation(_))

      if (toEmit.isEmpty) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      emitJavaAnnotations(jmember.getConstantPool, buf, toEmit)

      addAttribute(jmember, nme.RuntimeAnnotationATTR, buf)
    }

    def addParamAnnotations(jmethod: JMethod, pannotss: List[List[AnnotationInfo]]) {
      val annotations = pannotss map (annots => annots.filter(shouldEmitAnnotation(_)))
      if (annotations.forall(_.isEmpty)) return;

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      // number of parameters
      buf.put(annotations.length.toByte)
      for (annots <- annotations)
        emitJavaAnnotations(jmethod.getConstantPool, buf, annots)

      addAttribute(jmethod, nme.RuntimeParamAnnotationATTR, buf)
    }

    def addAttribute(jmember: JMember, name: Name, buf: ByteBuffer) {
      if (buf.position() < 2)
        return

      val length = buf.position();
      val arr = buf.array().slice(0, length);

      val attr = jmember.getContext().JOtherAttribute(jmember.getJClass(),
                                                      jmember,
                                                      name.toString,
                                                      arr,
                                                      length)
      jmember.addAttribute(attr)
    }

    def addInnerClasses(jclass: JClass) {
      def addOwnInnerClasses(cls: Symbol) {
        for (sym <- cls.info.decls.iterator if sym.isClass)
          innerClasses = innerClasses + sym;
      }
      // add inner classes which might not have been referenced yet
      atPhase(currentRun.erasurePhase) {
        addOwnInnerClasses(clasz.symbol)
        addOwnInnerClasses(clasz.symbol.linkedClassOfClass)
      }

      if (!innerClasses.isEmpty) {
        val innerClassesAttr = jclass.getInnerClasses()
        // sort them so inner classes succeed their enclosing class
        // to satisfy the Eclipse Java compiler
        for (innerSym <- innerClasses.toList.sort(_.name.length < _.name.length)) {
          var outerName = javaName(innerSym.rawowner)
          // remove the trailing '$'
          if (outerName.endsWith("$"))
            outerName = outerName.substring(0, outerName.length - 1)
          var flags = javaFlags(innerSym)
          if (innerSym.rawowner.hasFlag(Flags.MODULE))
            flags |= JAccessFlags.ACC_STATIC

          innerClassesAttr.addEntry(javaName(innerSym),
              outerName,
              innerSym.rawname.toString,
              flags);
        }
      }
    }

    def isTopLevelModule(sym: Symbol): Boolean =
      atPhase (currentRun.picklerPhase.next) {
        sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
      }

    def isStaticModule(sym: Symbol): Boolean = {
      sym.isModuleClass && !sym.isImplClass && !sym.hasFlag(Flags.LIFTED)
    }

    def genField(f: IField) {
      if (settings.debug.value)
        log("Adding field: " + f.symbol.fullNameString);
      var attributes = 0

      f.symbol.annotations foreach { a => a match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == TransientAtt =>
          attributes = attributes | JAccessFlags.ACC_TRANSIENT
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == VolatileAttr =>
          attributes = attributes | JAccessFlags.ACC_VOLATILE
        case _ => ();
      }}
      var flags = javaFlags(f.symbol)
      if (!f.symbol.hasFlag(Flags.MUTABLE))
        flags = flags | JAccessFlags.ACC_FINAL

      val jfield =
        jclass.addNewField(flags | attributes,
                           javaName(f.symbol),
                           javaType(f.symbol.tpe));
      addGenericSignature(jfield, f.symbol, clasz.symbol)
      addAnnotations(jfield, f.symbol.annotations)
    }

    def genMethod(m: IMethod) {
      log("Generating method " + m.symbol.fullNameString)
      method = m
      endPC.clear
      computeLocalVarsIndex(m)

      var resTpe = javaType(m.symbol.tpe.resultType)
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID;

      var flags = javaFlags(m.symbol)
      if (jclass.isInterface())
        flags = flags | JAccessFlags.ACC_ABSTRACT;

      // native methods of objects are generated in mirror classes
      if (method.native)
        flags = flags | JAccessFlags.ACC_NATIVE

      jmethod = jclass.addNewMethod(flags,
                                    javaName(m.symbol),
                                    resTpe,
                                    javaTypes(m.params map (_.kind)),
                                    javaNames(m.params map (_.sym)));

      addRemoteException(jmethod, m.symbol)

      if (!jmethod.isAbstract() && !method.native) {
        val jcode = jmethod.getCode().asInstanceOf[JExtendedCode]

        // add a fake local for debugging purpuses
        if (emitVars && isClosureApply(method.symbol)) {
          val outerField = clasz.symbol.info.decl(nme.getterToLocal(nme.OUTER))
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this = new Local(
              method.symbol.newVariable(NoPosition, "this$"), toTypeKind(outerField.tpe), false)
            m.locals = m.locals ::: List(_this)
            computeLocalVarsIndex(m) // since we added a new local, we need to recompute indexes

            jcode.emitALOAD_0
            jcode.emitGETFIELD(javaName(clasz.symbol),
                               javaName(outerField),
                               javaType(outerField))
            jcode.emitSTORE(indexOf(_this), javaType(_this.kind))
          }
        }

        for (local <- m.locals if ! m.params.contains(local)) {
          if (settings.debug.value)
            log("add local var: " + local);
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym))
        }

        genCode(m)
        if (emitVars)
          genLocalVariableTable(m, jcode);
      }

      addGenericSignature(jmethod, m.symbol, clasz.symbol)
      val (excs, others) = splitAnnotations(m.symbol.annotations, ThrowsAttr)
      addExceptionsAttribute(jmethod, excs)
      addAnnotations(jmethod, others)
      addParamAnnotations(jmethod, m.params.map(_.sym.annotations))
    }

    private def addRemoteException(jmethod: JMethod, meth: Symbol) {
      def isRemoteThrows(ainfo: AnnotationInfo) = ainfo match {
        case AnnotationInfo(tp, List(arg), _) if tp.typeSymbol == ThrowsAttr =>
          arg match {
            case Literal(Constant(tpe: Type)) if tpe.typeSymbol == RemoteException.typeSymbol => true
            case _ => false
          }
        case _ => false
      }

      if (remoteClass ||
          (meth.hasAnnotation(RemoteAttr) && jmethod.isPublic())) {
        val c = Constant(RemoteException)
        val ainfo = AnnotationInfo(ThrowsAttr.tpe, List(Literal(c).setType(c.tpe)), List())
        if (!meth.annotations.exists(isRemoteThrows)) {
          meth.addAnnotation(ainfo)
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
      sym.owner.hasFlag(Flags.SYNTHETIC) &&
      sym.owner.tpe.parents.exists { t =>
        val TypeRef(_, sym, _) = t;
        definitions.FunctionClass exists sym.==
      }
    }

    def addModuleInstanceField {
      import JAccessFlags._
      jclass.addNewField(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
                        nme.MODULE_INSTANCE_FIELD.toString,
                        jclass.getType())
    }

    def addStaticInit(cls: JClass) {
      import JAccessFlags._
      val clinitMethod = cls.addNewMethod(ACC_PUBLIC | ACC_STATIC,
                                          "<clinit>",
                                          JType.VOID,
                                          JType.EMPTY_ARRAY,
                                          new Array[String](0))
      val clinit = clinitMethod.getCode().asInstanceOf[JExtendedCode]
      if (isStaticModule(clasz.symbol)) {
        clinit.emitNEW(cls.getName())
        clinit.emitINVOKESPECIAL(cls.getName(),
                                 JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                 JMethodType.ARGLESS_VOID_FUNCTION)
      }

      serialVUID match {
        case Some(value) =>
          val fieldName = "serialVersionUID"
          jclass.addNewField(JAccessFlags.ACC_STATIC | JAccessFlags.ACC_PUBLIC | JAccessFlags.ACC_FINAL,
                             fieldName,
                             JType.LONG)
          clinit.emitPUSH(value)
          clinit.emitPUTSTATIC(jclass.getName(), fieldName, JType.LONG)
        case None => ()
      }

      if (clasz.bootstrapClass.isDefined) emitBootstrapMethodInstall(clinit)

      clinit.emitRETURN()
    }

    /** Emit code that installs a boostrap method for invoke dynamic. It installs the default
     *  method, found in scala.runtime.DynamicDispatch.
     */
    def emitBootstrapMethodInstall(jcode: JExtendedCode) {
      jcode.emitPUSH(jclass.getType.asInstanceOf[JReferenceType])
      jcode.emitPUSH(new JObjectType("scala.runtime.DynamicDispatch"))
      jcode.emitPUSH("bootstrapInvokeDynamic")
      jcode.emitGETSTATIC("java.dyn.Linkage", "BOOTSTRAP_METHOD_TYPE", MethodTypeType)
      jcode.emitDUP
      jcode.emitINVOKESTATIC("scala.Console", "println", new JMethodType(JType.VOID, Array(JObjectType.JAVA_LANG_OBJECT)))
      jcode.emitINVOKESTATIC("java.dyn.MethodHandles", "findStatic",
                              new JMethodType(MethodHandleType, Array(JavaLangClassType, JObjectType.JAVA_LANG_STRING, MethodTypeType)))
      jcode.emitINVOKESTATIC("java.dyn.Linkage", "registerBootstrapMethod",
                              new JMethodType(JType.VOID, Array(JavaLangClassType, MethodHandleType)))
    }

    /** Add a forwarder for method m */
    def addForwarder(jclass: JClass, module: Symbol, m: Symbol) {
      import JAccessFlags._
      val moduleName = javaName(module) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val paramJavaTypes = m.info.paramTypes map toTypeKind
      val paramNames: Array[String] = new Array[String](paramJavaTypes.length);

      for (i <- 0 until paramJavaTypes.length)
        paramNames(i) = "x_" + i

      val mirrorMethod = jclass.addNewMethod(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
        javaName(m),
        javaType(m.info.resultType),
        javaTypes(paramJavaTypes),
        paramNames);
      val mirrorCode = mirrorMethod.getCode().asInstanceOf[JExtendedCode];
      mirrorCode.emitGETSTATIC(moduleName,
                               nme.MODULE_INSTANCE_FIELD.toString,
                               new JObjectType(moduleName));
      var i = 0
      var index = 0
      var argTypes = mirrorMethod.getArgumentTypes()
      while (i < argTypes.length) {
        mirrorCode.emitLOAD(index, argTypes(i))
        index = index + argTypes(i).getSize()
        i += 1
      }

      mirrorCode.emitINVOKEVIRTUAL(moduleName, mirrorMethod.getName(), mirrorMethod.getType().asInstanceOf[JMethodType])
      mirrorCode.emitRETURN(mirrorMethod.getReturnType())

      addRemoteException(mirrorMethod, m)
      // only add generic signature if the method is concrete; bug #1745
      if (!m.hasFlag(Flags.DEFERRED))
        addGenericSignature(mirrorMethod, m, module)

      val (throws, others) = splitAnnotations(m.annotations, ThrowsAttr)
      addExceptionsAttribute(mirrorMethod, throws)
      addAnnotations(mirrorMethod, others)
      addParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))
    }

    /** Add forwarders for all methods defined in `module' that don't conflict with
     *  methods in the companion class of `module'. A conflict arises when a method
     *  with the same name is defined both in a class and its companion object (method
     *  signature is not taken into account).  If 3rd argument cond is supplied, only
     *  symbols for which cond(sym) is true are given forwarders.
     */
    def addForwarders(jclass: JClass, module: Symbol) { addForwarders(jclass, module, _ => true) }
    def addForwarders(jclass: JClass, module: Symbol, cond: (Symbol) => Boolean) {
      def conflictsIn(cls: Symbol, name: Name) =
        cls.info.nonPrivateMembers.exists(_.name == name)

      /** List of parents shared by both class and module, so we don't add forwarders
       *  for methods defined there - bug #1804 */
      lazy val commonParents = {
        val cps = module.info.baseClasses
        val mps = module.linkedClassOfModule.info.baseClasses
        cps.filter(mps contains)
      }
      /* the setter doesn't show up in members so we inspect the name */
      def conflictsInCommonParent(name: Name) =
        commonParents exists { cp => name startsWith (cp.name + "$") }

      /** Should method `m' get a forwarder in the mirror class? */
      def shouldForward(m: Symbol): Boolean =
        atPhase(currentRun.picklerPhase) (
          m.owner != definitions.ObjectClass
          && m.isMethod
          && !m.hasFlag(Flags.CASE | Flags.PROTECTED)
          && !m.isConstructor
          && !m.isStaticMember
          && !(m.owner == definitions.AnyClass)
          && !module.isSubClass(module.linkedClassOfModule)
          && !conflictsIn(definitions.ObjectClass, m.name)
          && !conflictsInCommonParent(m.name)
          && !conflictsIn(module.linkedClassOfModule, m.name)
        )

      assert(module.isModuleClass)
      if (settings.debug.value)
        log("Dumping mirror class for object: " + module);

      for (m <- module.info.nonPrivateMembers; if shouldForward(m) ; if cond(m))
        addForwarder(jclass, module, m)
    }

    /** Dump a mirror class for a top-level module. A mirror class is a class containing
     *  only static methods that forward to the corresponding method on the MODULE instance
     *  of the given Scala object.
     */
    def dumpMirrorClass(clasz: Symbol, sourceFile: String) {
      import JAccessFlags._
      val moduleName = javaName(clasz) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           "java.lang.Object",
                                           JClass.NO_INTERFACES,
                                           sourceFile)
      addForwarders(mirrorClass, clasz)
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
          log("Making labels for: " + method);
        HashMap.empty ++ bs.zip(bs map (b => jcode.newLabel))
      }

      isModuleInitialized = false

      linearization = linearizer.linearize(m)
      val labels = makeLabels(linearization)
      /** local variables whose scope appears in this block. */
      var varsInBlock: collection.mutable.Set[Local] = new HashSet

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

        linearization foreach ((b) => {
          if (! (covered contains b) ) {
            if (start >= 0) { // we're inside a handler range
              end = labels(b).getAnchor()
              ranges = (start, end) :: ranges
              start = -1
            }
          } else {
            if (start >= 0) { // we're inside a handler range
              end = endPC(b)
            } else {
              start = labels(b).getAnchor()
              end   = endPC(b)
            }
            covered = covered - b
          }
        });

        /* Add the last interval. Note that since the intervals are
         * open-ended to the right, we have to give a number past the actual
         * code!
         */
        if (start >= 0) {
          ranges = (start, jcode.getPC()) :: ranges;
        }

        if (!covered.isEmpty)
          if (settings.debug.value)
            log("Some covered blocks were not found in method: " + method +
                " covered: " + covered + " not in " + linearization);
        ranges
      }

      this.method.exh foreach { e =>
        ranges(e).sort({ (p1, p2) => p1._1 < p2._1 })
        .foreach { p =>
          if (p._1 < p._2) {
            if (settings.debug.value)
              log("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
                  " from: " + p._1 + " to: " + p._2 + " catching: " + e.cls);
            jcode.addExceptionHandler(p._1, p._2,
                                      labels(e.startBlock).getAnchor(),
                                      if (e.cls == NoSymbol) null else javaName(e.cls))
          } else
            log("Empty exception range: " + p)
        }
      }
    }

    def genBlock(b: BasicBlock) {
      labels(b).anchorToNext()

      if (settings.debug.value)
        log("Generating code for block: " + b + " at pc: " + labels(b).getAnchor());
      var lastMappedPC = 0
      var lastLineNr = 0
      var crtPC = 0
      varsInBlock.clear

      for (instr <- b) {
        class CompilationError(msg: String) extends Error {
          override def toString: String = {
            msg +
            "\nCurrent method: " + method +
            "\nCurrent block: " + b +
            "\nCurrent instruction: " + instr +
            "\n---------------------" +
            method.dump
          }
        }
        def assert(cond: Boolean, msg: String) = if (!cond) throw new CompilationError(msg);

        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0()

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ();
              case BooleanTag => jcode.emitPUSH(const.booleanValue)
              case ByteTag    => jcode.emitPUSH(const.byteValue)
              case ShortTag   => jcode.emitPUSH(const.shortValue)
              case CharTag    => jcode.emitPUSH(const.charValue)
              case IntTag     => jcode.emitPUSH(const.intValue)
              case LongTag    => jcode.emitPUSH(const.longValue)
              case FloatTag   => jcode.emitPUSH(const.floatValue)
              case DoubleTag  => jcode.emitPUSH(const.doubleValue)
              case StringTag  => jcode.emitPUSH(const.stringValue)
              case NullTag    => jcode.emitACONST_NULL()
              case ClassTag   =>
                val kind = toTypeKind(const.typeValue);
                if (kind.isValueType)
                  jcode.emitPUSH(classLiteral(kind));
                else
                  jcode.emitPUSH(javaType(kind).asInstanceOf[JReferenceType]);
              case EnumTag   =>
                val sym = const.symbolValue
                jcode.emitGETSTATIC(javaName(sym.owner),
                                    javaName(sym),
                                    javaType(sym.tpe.underlying))
              case _          => abort("Unknown constant value: " + const);
            }

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind))

          case LOAD_LOCAL(local) =>
            jcode.emitLOAD(indexOf(local), javaType(local.kind))

          case LOAD_FIELD(field, isStatic) =>
            var owner = javaName(field.owner);
//            if (field.owner.hasFlag(Flags.MODULE)) owner = owner + "$";
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
              log("genearting LOAD_MODULE for: " + module + " flags: " +
                  Flags.flagsToString(module.flags));
            if (clasz.symbol == module.moduleClass && jmethod.getName() != nme.readResolve.toString)
              jcode.emitALOAD_0()
            else
              jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                  nme.MODULE_INSTANCE_FIELD.toString,
                                  javaType(module));

          case STORE_ARRAY_ITEM(kind) =>
            jcode.emitASTORE(javaType(kind))

          case STORE_LOCAL(local) =>
            jcode.emitSTORE(indexOf(local), javaType(local.kind))

          case STORE_THIS(_) =>
            // this only works for impl classes because the self parameter comes first
            // in the method signature. If that changes, this code has to be revisited.
            jcode.emitASTORE_0()

          case STORE_FIELD(field, isStatic) =>
            val owner = javaName(field.owner) // + (if (field.owner.hasFlag(Flags.MODULE)) "$" else "");
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

          case call @ CALL_METHOD(method, style) =>
            val owner: String = javaName(method.owner);
            //reference the type of the receiver instead of the method owner (if not an interface!)
            val dynamicOwner =
              if (needsInterfaceCall(call.hostClass)) owner
              else javaName(call.hostClass)

            style match {
              case InvokeDynamic =>
                jcode.emitINVOKEINTERFACE("java.dyn.Dynamic",
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType])

              case Dynamic =>
                if (needsInterfaceCall(method.owner))
                  jcode.emitINVOKEINTERFACE(owner,
                                            javaName(method),
                                            javaType(method).asInstanceOf[JMethodType])
                else
                  jcode.emitINVOKEVIRTUAL(dynamicOwner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case Static(instance) =>
                if (instance) {
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                } else
                  jcode.emitINVOKESTATIC(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case SuperCall(_) =>
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                  // we initialize the MODULE$ field immediately after the super ctor
                  if (isStaticModule(clasz.symbol) && !isModuleInitialized &&
                      jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
                      javaName(method) == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
                        isModuleInitialized = true;
                        jcode.emitALOAD_0();
                        jcode.emitPUTSTATIC(jclass.getName(),
                                            nme.MODULE_INSTANCE_FIELD.toString,
                                            jclass.getType());
                      }


            }

          case BOX(kind) =>
            val boxedType = definitions.boxedClass(kind.toType.typeSymbol)
            val mtype = new JMethodType(javaType(boxedType), Array(javaType(kind)))
            jcode.emitINVOKESTATIC(BoxesRunTime, "boxTo" + boxedType.cleanNameString, mtype)

          case UNBOX(kind) =>
            val mtype = new JMethodType(javaType(kind), Array(JObjectType.JAVA_LANG_OBJECT))
            jcode.emitINVOKESTATIC(BoxesRunTime, "unboxTo" + kind.toType.typeSymbol.cleanNameString, mtype)

          case NEW(REFERENCE(cls)) =>
            val className = javaName(cls)
            jcode.emitNEW(className)

          case CREATE_ARRAY(elem, 1) => elem match {
            case REFERENCE(_) | ARRAY(_) =>
              jcode.emitANEWARRAY(javaType(elem).asInstanceOf[JReferenceType])
            case _ =>
              jcode.emitNEWARRAY(javaType(elem))
          }
          case CREATE_ARRAY(elem, dims) =>
            jcode.emitMULTIANEWARRAY(javaType(ArrayN(elem, dims)).asInstanceOf[JReferenceType], dims)

          case IS_INSTANCE(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitINSTANCEOF(new JObjectType(javaName(cls)))
              case ARRAY(elem)    => jcode.emitINSTANCEOF(new JArrayType(javaType(elem)))
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case CHECK_CAST(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitCHECKCAST(new JObjectType(javaName(cls)))
              case ARRAY(elem)    => jcode.emitCHECKCAST(new JArrayType(javaType(elem)))
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case SWITCH(tags, branches) =>
            val tagArray = new Array[Array[Int]](tags.length)
            var caze = tags
            var i = 0

            while (i < tagArray.length) {
              tagArray(i) = new Array[Int](caze.head.length)
              caze.head.copyToArray(tagArray(i), 0)
              i = i + 1
              caze = caze.tail
            }
            val branchArray = jcode.newLabels(tagArray.length)
            i = 0
            while (i < branchArray.length) {
              branchArray(i) = labels(branches(i))
              i += 1
            }
            if (settings.debug.value)
              log("Emitting SWITHCH:\ntags: " + tags + "\nbranches: " + branches);
            jcode.emitSWITCH(tagArray,
                             branchArray,
                             labels(branches.last),
                             MIN_SWITCH_DENSITY);
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
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
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
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                val Success = success
                val Failure = failure
                (cond, nextBlock) match {
                  case (EQ, Success) =>
                    jcode.emitIFNONNULL(labels(failure))
                  case (NE, Failure) =>
                    jcode.emitIFNONNULL(labels(success))
                  case (EQ, Failure) =>
                    jcode.emitIFNULL(labels(success))
                  case (NE, Success) =>
                    jcode.emitIFNULL(labels(failure))
                  case (EQ, _) =>
                    jcode.emitIFNULL(labels(success));
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                  case (NE, _) =>
                    jcode.emitIFNONNULL(labels(success));
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   => jcode.emitLCONST_0(); jcode.emitLCMP()
                  case FLOAT  =>
                    jcode.emitFCONST_0();
                    if (cond == LT || cond == LE) jcode.emitFCMPG()
                    else jcode.emitFCMPL()
                  case DOUBLE =>
                    jcode.emitDCONST_0();
                    if (cond == LT || cond == LE) jcode.emitDCMPG()
                    else jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case RETURN(kind) =>
            jcode.emitRETURN(javaType(kind))

          case THROW() =>
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
            if (varsInBlock contains lv) {
              lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
              varsInBlock -= lv
            } else if (b.varsInScope contains lv) {
              lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
              b.varsInScope -= lv
            } else
              assert(false, "Illegal local var nesting: " + method)

          case LOAD_EXCEPTION() =>
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
          endPC(b) = jcode.getPC();

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
          jcode.emitNEW(StringBuilderClass)
          jcode.emitDUP()
          jcode.emitINVOKESPECIAL(StringBuilderClass,
                                  JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                  JMethodType.ARGLESS_VOID_FUNCTION)

        case StringConcat(el) =>
          val jtype = el match {
            case REFERENCE(_) | ARRAY(_)=> JObjectType.JAVA_LANG_OBJECT
            case _ => javaType(el)
          }
          jcode.emitINVOKEVIRTUAL(StringBuilderClass,
                                  "append",
                                  new JMethodType(StringBuilderType,
                                  Array(jtype)))
        case EndConcat =>
          jcode.emitINVOKEVIRTUAL(StringBuilderClass,
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
        var vars = m.locals.filter(l => !l.sym.hasFlag(Flags.SYNTHETIC))

        if (vars.length == 0) return

        val pool = jclass.getConstantPool()
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
          lvTab.putShort(pool.addUtf8(name).asInstanceOf[Short])
          lvTab.putShort(pool.addUtf8(signature).asInstanceOf[Short])
          lvTab.putShort(idx)
        }

        lvTab.putShort(entries.asInstanceOf[Short])

        if (!jmethod.isStatic()) {
          emitEntry("this", jclass.getType().getSignature(), 0, 0.asInstanceOf[Short], pc.asInstanceOf[Short])
        }

        for (lv <- vars) {
            val name = if (javaName(lv.sym) eq null) {
              anonCounter += 1
              "<anon" + anonCounter + ">"
            } else javaName(lv.sym)

            val index = indexOf(lv).asInstanceOf[Short]
            val tpe   = javaType(lv.kind).getSignature()
            for ((start, end) <- lv.ranges) {
              emitEntry(name, tpe, index, start.asInstanceOf[Short], (end - start).asInstanceOf[Short])
            }
        }
        val attr =
            fjbgContext.JOtherAttribute(jclass,
                                        jmethod,
                                        nme.LocalVariableTableATTR.toString,
                                        lvTab.array())
        jcode.addAttribute(attr)
    }


    /** For each basic block, the first PC address following it. */
    val endPC: HashMap[BasicBlock, Int] = new HashMap()
    val conds: HashMap[TestOp, Int] = new HashMap()

    conds += (EQ -> JExtendedCode.COND_EQ)
    conds += (NE -> JExtendedCode.COND_NE)
    conds += (LT -> JExtendedCode.COND_LT)
    conds += (GT -> JExtendedCode.COND_GT)
    conds += (LE -> JExtendedCode.COND_LE)
    conds += (GE -> JExtendedCode.COND_GE)

    val negate: HashMap[TestOp, TestOp] = new HashMap()

    negate += (EQ -> NE)
    negate += (NE -> EQ)
    negate += (LT -> GE)
    negate += (GT -> LE)
    negate += (LE -> GT)
    negate += (GE -> LT)

    /** Map from type kinds to the Java reference types. It is used for
     *  loading class constants. @see Predef.classOf. */
    val classLiteral: Map[TypeKind, JObjectType] = new HashMap()

    classLiteral += (UNIT   -> new JObjectType("java.lang.Void"))
    classLiteral += (BOOL   -> new JObjectType("java.lang.Boolean"))
    classLiteral += (BYTE   -> new JObjectType("java.lang.Byte"))
    classLiteral += (SHORT  -> new JObjectType("java.lang.Short"))
    classLiteral += (CHAR   -> new JObjectType("java.lang.Character"))
    classLiteral += (INT    -> new JObjectType("java.lang.Integer"))
    classLiteral += (LONG   -> new JObjectType("java.lang.Long"))
    classLiteral += (FLOAT  -> new JObjectType("java.lang.Float"))
    classLiteral += (DOUBLE -> new JObjectType("java.lang.Double"))


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
             "Invalid index for: " + local + "{" + local.hashCode + "}: ")
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
          log("Index value for " + l + "{" + l.hashCode + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }
    }

    ////////////////////// Utilities ////////////////////////

    /**
     * <p>
     *   Return the a name of this symbol that can be used on the Java
     *   platform. It removes spaces from names.
     * </p>
     * <p>
     *   Special handling: scala.Nothing and <code>scala.Null</code> are
     *   <em>erased</em> to <code>scala.runtime.Nothing$</code> and
     *   </code>scala.runtime.Null$</code>. This is needed because they are
     *   not real classes, and they mean 'abrupt termination upon evaluation
     *   of that expression' or <code>null</code> respectively. This handling is
     *   done already in <a href="../icode/GenIcode.html" target="contentFrame">
     *   <code>GenICode</code></a>, but here we need to remove references
     *   from method signatures to these types, because such classes can
     *   not exist in the classpath: the type checker will be very confused.
     * </p>
     */
    def javaName(sym: Symbol): String = {
      val suffix = if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
                        !sym.isImplClass &&
                        !sym.hasFlag(Flags.JAVA)) "$" else "";

      if (sym == definitions.NothingClass)
        return javaName(definitions.RuntimeNothingClass)
      else if (sym == definitions.NullClass)
        return javaName(definitions.RuntimeNullClass)

      if (sym.isClass && !sym.rawowner.isPackageClass && !sym.isModuleClass) {
        innerClasses = innerClasses + sym;
      }

      (if (sym.isClass || (sym.isModule && !sym.isMethod))
        sym.fullNameString('/')
      else
        sym.simpleName.toString.trim()) + suffix
    }

    def javaNames(syms: List[Symbol]): Array[String] = {
      val res = new Array[String](syms.length)
      var i = 0
      syms foreach (s => { res(i) = javaName(s); i += 1 })
      res
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
      import JAccessFlags._

      var jf: Int = 0
      val f = sym.flags
      jf = jf | (if (sym hasFlag Flags.SYNTHETIC) ACC_SYNTHETIC else 0)
/*      jf = jf | (if (sym hasFlag Flags.PRIVATE) ACC_PRIVATE else
                  if (sym hasFlag Flags.PROTECTED) ACC_PROTECTED else ACC_PUBLIC)
*/
      jf = jf | (if (sym hasFlag Flags.PRIVATE) ACC_PRIVATE else  ACC_PUBLIC)
      jf = jf | (if ((sym hasFlag Flags.ABSTRACT) ||
                     (sym hasFlag Flags.DEFERRED)) ACC_ABSTRACT else 0)
      jf = jf | (if (sym hasFlag Flags.INTERFACE) ACC_INTERFACE else 0)
      jf = jf | (if ((sym hasFlag Flags.FINAL)
                       && !sym.enclClass.hasFlag(Flags.INTERFACE)
                       && !sym.isClassConstructor) ACC_FINAL else 0)
      jf = jf | (if (sym.isStaticMember) ACC_STATIC else 0)
      jf = jf | (if (sym hasFlag Flags.BRIDGE) ACC_BRIDGE | ACC_SYNTHETIC else 0)

      if (sym.isClass && !sym.hasFlag(Flags.INTERFACE))
        jf = jf | ACC_SUPER

      // constructors of module classes should be private
      if (sym.isPrimaryConstructor && isTopLevelModule(sym.owner)) {
        jf |= ACC_PRIVATE
        jf &= ~ACC_PUBLIC
      }
      jf
    }

    /** Calls to methods in 'sym' need invokeinterface? */
    def needsInterfaceCall(sym: Symbol): Boolean = {
      log("checking for interface call: " + sym.fullNameString)
      // the following call to 'info' may cause certain symbols to fail loading because we're
      // too late in the compilation chain (aliases to overloaded symbols will not be properly
      // resolved, see scala.Range, method super$++ that fails in UnPickler at LazyTypeRefAndAlias.complete
      if (sym.isTrait) sym.info // needed so that the type is up to date (erasure may add lateINTERFACE to traits)

      sym.hasFlag(Flags.INTERFACE) ||
      (sym.hasFlag(Flags.JAVA) &&
       sym.isNonBottomSubClass(definitions.ClassfileAnnotationClass))
    }


    def javaType(t: TypeKind): JType = (t: @unchecked) match {
      case UNIT            => JType.VOID
      case BOOL            => JType.BOOLEAN
      case BYTE            => JType.BYTE
      case SHORT           => JType.SHORT
      case CHAR            => JType.CHAR
      case INT             => JType.INT
      case LONG            => JType.LONG
      case FLOAT           => JType.FLOAT
      case DOUBLE          => JType.DOUBLE
      case REFERENCE(cls)  => new JObjectType(javaName(cls))
      case ARRAY(elem)     => new JArrayType(javaType(elem))
    }

    def javaType(t: Type): JType = javaType(toTypeKind(t))

    def javaType(s: Symbol): JType =
      if (s.isMethod)
        new JMethodType(
          if (s.isClassConstructor) JType.VOID else javaType(s.tpe.resultType),
          s.tpe.paramTypes.map(javaType).toArray)
      else
        javaType(s.tpe)

    def javaTypes(ts: List[TypeKind]): Array[JType] = {
      val res = new Array[JType](ts.length)
      var i = 0
      ts foreach ( t => { res(i) = javaType(t); i += 1 } );
      res
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
      throw new Error(msg + "\nMethod: " + method)
    }

    def assert(cond: Boolean) { assert(cond, "Assertion failed.") }
  }
}
