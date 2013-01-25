/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Iulian Dragos
 */

package scala.tools.nsc
package backend.jvm

import java.io.{ByteArrayOutputStream, DataOutputStream, OutputStream }
import java.nio.ByteBuffer
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.pickling.{ PickleFormat, PickleBuffer }
import scala.tools.nsc.symtab._
import scala.reflect.internal.util.{ SourceFile, NoSourceFile }
import scala.reflect.internal.ClassfileConstants._
import ch.epfl.lamp.fjbg._
import JAccessFlags._
import JObjectType.{ JAVA_LANG_STRING, JAVA_LANG_OBJECT }
import java.util.jar.{ JarEntry, JarOutputStream }
import scala.tools.nsc.io.AbstractFile
import scala.language.postfixOps

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 *
 */
abstract class GenJVM extends SubComponent with GenJVMUtil with GenAndroid with BytecodeWriters with GenJVMASM {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions._

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase): Phase = new JvmPhase(p)

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName
    override def erasedTypes = true
    def apply(cls: IClass) = sys.error("no implementation")

    override def run() {
      // we reinstantiate the bytecode generator at each run, to allow the GC
      // to collect everything
      if (settings.debug.value)
        inform("[running phase " + name + " on icode]")

      if (settings.Xdce.value)
        for ((sym, cls) <- icodes.classes if inliner.isClosureClass(sym) && !deadCode.liveClosures(sym)) {
          log(s"Optimizer eliminated ${sym.fullNameString}")
          icodes.classes -= sym
        }

      // For predictably ordered error messages.
      val sortedClasses = classes.values.toList sortBy ("" + _.symbol.fullName)
      val entryPoints   = sortedClasses filter isJavaEntryPoint

      val bytecodeWriter = settings.outputDirs.getSingleOutput match {
        case Some(f) if f hasExtension "jar" =>
          // If no main class was specified, see if there's only one
          // entry point among the classes going into the jar.
          if (settings.mainClass.isDefault) {
            entryPoints map (_.symbol fullName '.') match {
              case Nil      =>
                log("No Main-Class designated or discovered.")
              case name :: Nil =>
                log("Unique entry point: setting Main-Class to " + name)
                settings.mainClass.value = name
              case names =>
                log("No Main-Class due to multiple entry points:\n  " + names.mkString("\n  "))
            }
          }
          else log("Main-Class was specified: " + settings.mainClass.value)

          new DirectToJarfileWriter(f.file)

        case _                               =>
          if (settings.Ygenjavap.isDefault) {
            if(settings.Ydumpclasses.isDefault)
              new ClassBytecodeWriter { }
            else
              new ClassBytecodeWriter with DumpBytecodeWriter { }
          }
          else new ClassBytecodeWriter with JavapBytecodeWriter { }
      }

      val codeGenerator = new BytecodeGenerator(bytecodeWriter)
      debuglog("Created new bytecode generator for " + classes.size + " classes.")

      sortedClasses foreach { c =>
        try codeGenerator.genClass(c)
        catch {
          case e: JCode.CodeSizeTooBigException =>
            log("Skipped class %s because it has methods that are too long.".format(c))
        }
      }

      bytecodeWriter.close()
      classes.clear()
    }
  }

  var pickledBytes = 0 // statistics

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator(bytecodeWriter: BytecodeWriter) extends BytecodeUtil {
    def this() = this(new ClassBytecodeWriter { })
    def debugLevel = settings.debuginfo.indexOfChoice
    import bytecodeWriter.writeClass

    val MIN_SWITCH_DENSITY = 0.7
    val INNER_CLASSES_FLAGS =
      (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED | ACC_STATIC | ACC_INTERFACE | ACC_ABSTRACT)

    val PublicStatic      = ACC_PUBLIC | ACC_STATIC
    val PublicStaticFinal = ACC_PUBLIC | ACC_STATIC | ACC_FINAL

    val StringBuilderClassName = javaName(definitions.StringBuilderClass)
    val BoxesRunTime = "scala.runtime.BoxesRunTime"

    val StringBuilderType = new JObjectType(StringBuilderClassName)               // TODO use ASMType.getObjectType
    val toStringType      = new JMethodType(JAVA_LANG_STRING, JType.EMPTY_ARRAY)  // TODO use ASMType.getMethodType
    val arrayCloneType    = new JMethodType(JAVA_LANG_OBJECT, JType.EMPTY_ARRAY)
    val MethodTypeType    = new JObjectType("java.dyn.MethodType")
    val JavaLangClassType = new JObjectType("java.lang.Class")
    val MethodHandleType  = new JObjectType("java.dyn.MethodHandle")

    // Scala attributes
    val BeanInfoAttr        = rootMirror.getRequiredClass("scala.beans.BeanInfo")
    val BeanInfoSkipAttr    = rootMirror.getRequiredClass("scala.beans.BeanInfoSkip")
    val BeanDisplayNameAttr = rootMirror.getRequiredClass("scala.beans.BeanDisplayName")
    val BeanDescriptionAttr = rootMirror.getRequiredClass("scala.beans.BeanDescription")

    // Additional interface parents based on annotations and other cues
    def newParentForAttr(attr: Symbol): Option[Symbol] = attr match {
      case SerializableAttr => Some(SerializableClass)
      case CloneableAttr    => Some(JavaCloneableClass)
      case RemoteAttr       => Some(RemoteInterfaceClass)
      case _                => None
    }

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    private def helperBoxTo(kind: ValueTypeKind): Tuple2[String, JMethodType] = {
      val boxedType = definitions.boxedClass(kind.toType.typeSymbol)
      val mtype = new JMethodType(javaType(boxedType), Array(javaType(kind)))

      Pair("boxTo" + boxedType.decodedName, mtype)
    }

    private val jBoxTo: Map[TypeKind, Tuple2[String, JMethodType]] = Map(
      BOOL   -> helperBoxTo(BOOL)  ,
      BYTE   -> helperBoxTo(BYTE)  ,
      CHAR   -> helperBoxTo(CHAR)  ,
      SHORT  -> helperBoxTo(SHORT) ,
      INT    -> helperBoxTo(INT)   ,
      LONG   -> helperBoxTo(LONG)  ,
      FLOAT  -> helperBoxTo(FLOAT) ,
      DOUBLE -> helperBoxTo(DOUBLE)
    )

    private def helperUnboxTo(kind: ValueTypeKind): Tuple2[String, JMethodType] = {
      val mtype = new JMethodType(javaType(kind), Array(JAVA_LANG_OBJECT))
      val mname = "unboxTo" + kind.toType.typeSymbol.decodedName

      Pair(mname, mtype)
    }

    private val jUnboxTo: Map[TypeKind, Tuple2[String, JMethodType]] = Map(
      BOOL   -> helperUnboxTo(BOOL)  ,
      BYTE   -> helperUnboxTo(BYTE)  ,
      CHAR   -> helperUnboxTo(CHAR)  ,
      SHORT  -> helperUnboxTo(SHORT) ,
      INT    -> helperUnboxTo(INT)   ,
      LONG   -> helperUnboxTo(LONG)  ,
      FLOAT  -> helperUnboxTo(FLOAT) ,
      DOUBLE -> helperUnboxTo(DOUBLE)
    )

    var clasz: IClass = _
    var method: IMethod = _
    var jclass: JClass = _
    var jmethod: JMethod = _
    // var jcode: JExtendedCode = _

    def isParcelableClass = isAndroidParcelableClass(clasz.symbol)
    def isRemoteClass = clasz.symbol hasAnnotation RemoteAttr
    def serialVUID = clasz.symbol getAnnotation SerialVersionUIDAttr collect {
      case AnnotationInfo(_, Literal(const) :: _, _) => const.longValue
    }

    val fjbgContext = new FJBGContext(49, 0)

    val emitSource = debugLevel >= 1
    val emitLines  = debugLevel >= 2
    val emitVars   = debugLevel >= 3

    // bug had phase with wrong name; leaving enabled for brief pseudo deprecation
    private val checkSignatures = (
         (settings.check containsName phaseName)
      || (settings.check.value contains "genjvm") && {
            global.warning("This option will be removed: please use -Ycheck:%s, not -Ycheck:genjvm." format phaseName)
            true
         }
    )

    /** For given symbol return a symbol corresponding to a class that should be declared as inner class.
     *
     *  For example:
     *  class A {
     *    class B
     *    object C
     *  }
     *
     *  then method will return NoSymbol for A, the same symbol for A.B (corresponding to A$B class) and A$C$ symbol
     *  for A.C.
     */
    private def innerClassSymbolFor(s: Symbol): Symbol =
      if (s.isClass) s else if (s.isModule) s.moduleClass else NoSymbol

    override def javaName(sym: Symbol): String = { // TODO Miguel says: check whether a single pass over `icodes.classes` can populate `innerClassBuffer` faster.
      /**
       * Checks if given symbol corresponds to inner class/object and add it to innerClassBuffer
       *
       * Note: This method is called recursively thus making sure that we add complete chain
       * of inner class all until root class.
       */
      def collectInnerClass(s: Symbol): Unit = {
        // TODO: some beforeFlatten { ... } which accounts for
        // being nested in parameterized classes (if we're going to selectively flatten.)
        val x = innerClassSymbolFor(s)
        if(x ne NoSymbol) {
          assert(x.isClass, "not an inner-class symbol")
          val isInner = !x.rawowner.isPackageClass
          if (isInner) {
            innerClassBuffer += x
            collectInnerClass(x.rawowner)
          }
        }
      }
      collectInnerClass(sym)

      super.javaName(sym)
    }

    /** Write a class to disk, adding the Scala signature (pickled type
     *  information) and inner classes.
     *
     * @param jclass The FJBG class, where code was emitted
     * @param sym    The corresponding symbol, used for looking up pickled information
     */
    def emitClass(jclass: JClass, sym: Symbol) {
      addInnerClasses(jclass)
      writeClass("" + sym.name, jclass.getName(), toByteArray(jclass), sym)
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
        case Some(pickle) if !nme.isModuleName(newTermName(jclass.getName)) =>
          val scalaAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaSignatureATTR.toString,
                                        versionPickle.bytes, versionPickle.writeIndex)
          jclass addAttribute scalaAttr
          val scalaAnnot = {
            val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
            AnnotationInfo(sigBytes.sigAnnot, Nil, List((nme.bytes, sigBytes)))
          }
          pickledBytes += pickle.writeIndex
          currentRun.symData -= sym
          currentRun.symData -= sym.companionSymbol
          Some(scalaAnnot)
        case _ =>
          val markerAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaATTR.toString, new Array[Byte](0), 0)
          jclass addAttribute markerAttr
          None
      }

    private var innerClassBuffer = mutable.LinkedHashSet[Symbol]()

    /** Drop redundant interfaces (ones which are implemented by some other parent) from the immediate parents.
     *  This is important on Android because there is otherwise an interface explosion.
     */
    private def minimizeInterfaces(interfaces: List[Symbol]): List[Symbol] = {
      var rest   = interfaces
      var leaves = List.empty[Symbol]
      while(!rest.isEmpty) {
        val candidate = rest.head
        val nonLeaf = leaves exists { lsym => lsym isSubClass candidate }
        if(!nonLeaf) {
          leaves = candidate :: (leaves filterNot { lsym => candidate isSubClass lsym })
        }
        rest = rest.tail
      }

      leaves
    }

    def genClass(c: IClass) {
      clasz = c
      innerClassBuffer.clear()

      val name    = javaName(c.symbol)

      val ps = c.symbol.info.parents

      val superClass: Symbol = if(ps.isEmpty) ObjectClass else ps.head.typeSymbol;

      val superInterfaces0: List[Symbol] = if(ps.isEmpty) Nil else c.symbol.mixinClasses;
      val superInterfaces = superInterfaces0 ++ c.symbol.annotations.flatMap(ann => newParentForAttr(ann.symbol)) distinct

      val ifaces =
        if(superInterfaces.isEmpty) JClass.NO_INTERFACES
        else mkArray(minimizeInterfaces(superInterfaces) map javaName)

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(superClass),
                                  ifaces,
                                  c.cunit.source.toString)

      if (isStaticModule(c.symbol) || serialVUID != None || isParcelableClass) {
        if (isStaticModule(c.symbol))
          addModuleInstanceField
        addStaticInit(jclass, c.lookupStaticCtor)

        if (isTopLevelModule(c.symbol)) {
          if (c.symbol.companionClass == NoSymbol)
            generateMirrorClass(c.symbol, c.cunit.source)
          else
            log("No mirror class for module with linked class: " +
                c.symbol.fullName)
        }
      }
      else {
        c.lookupStaticCtor foreach (constructor => addStaticInit(jclass, Some(constructor)))

        // it must be a top level class (name contains no $s)
        def isCandidateForForwarders(sym: Symbol): Boolean =
          afterPickler {
            !(sym.name.toString contains '$') && sym.hasModuleFlag && !sym.isImplClass && !sym.isNestedClass
          }

        // At some point this started throwing lots of exceptions as a compile was finishing.
        // error: java.lang.AssertionError:
        //   assertion failed: List(object package$CompositeThrowable, object package$CompositeThrowable)
        // ...is the one I've seen repeatedly.  Suppressing.
        val lmoc = (
          try c.symbol.companionModule
          catch { case x: AssertionError =>
            Console.println("Suppressing failed assert: " + x)
            NoSymbol
          }
        )
        // add static forwarders if there are no name conflicts; see bugs #363 and #1735
        if (lmoc != NoSymbol && !c.symbol.isInterface) {
          if (isCandidateForForwarders(lmoc) && !settings.noForwarders.value) {
            log("Adding static forwarders from '%s' to implementations in '%s'".format(c.symbol, lmoc))
            addForwarders(jclass, lmoc.moduleClass)
          }
        }
      }

      clasz.fields foreach genField
      clasz.methods foreach genMethod

      val ssa = scalaSignatureAddingMarker(jclass, c.symbol)
      addGenericSignature(jclass, c.symbol, c.symbol.owner)
      addAnnotations(jclass, c.symbol.annotations ++ ssa)
      addEnclosingMethodAttribute(jclass, c.symbol)
      emitClass(jclass, c.symbol)

      if (c.symbol hasAnnotation BeanInfoAttr)
        genBeanInfoClass(c)
    }

    private def addEnclosingMethodAttribute(jclass: JClass, clazz: Symbol) {
      val sym = clazz.originalEnclosingMethod
      if (sym.isMethod) {
        debuglog("enclosing method for %s is %s (in %s)".format(clazz, sym, sym.enclClass))
        jclass addAttribute fjbgContext.JEnclosingMethodAttribute(
          jclass,
          javaName(sym.enclClass),
          javaName(sym),
          javaType(sym)
        )
      } else if (clazz.isAnonymousClass) {
        val enclClass = clazz.rawowner
        assert(enclClass.isClass, enclClass)
        val sym = enclClass.primaryConstructor
        if (sym == NoSymbol)
          log("Ran out of room looking for an enclosing method for %s: no constructor here.".format(
            enclClass, clazz)
          )
        else {
          debuglog("enclosing method for %s is %s (in %s)".format(clazz, sym, enclClass))
          jclass addAttribute fjbgContext.JEnclosingMethodAttribute(
            jclass,
            javaName(enclClass),
            javaName(sym),
            javaType(sym).asInstanceOf[JMethodType]
          )
        }
      }
    }

    private def toByteArray(jc: JClass): Array[Byte] = {
      val bos = new java.io.ByteArrayOutputStream()
      val dos = new java.io.DataOutputStream(bos)
      jc.writeTo(dos)
      dos.close()
      bos.toByteArray
    }

    /**
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     */
    def genBeanInfoClass(c: IClass) {
      val description = c.symbol getAnnotation BeanDescriptionAttr
      // informProgress(description.toString)

      val beanInfoClass = fjbgContext.JClass(javaFlags(c.symbol),
            javaName(c.symbol) + "BeanInfo",
            "scala/beans/ScalaBeanInfo",
            JClass.NO_INTERFACES,
            c.cunit.source.toString)

      var fieldList = List[String]()
      for (f <- clasz.fields if f.symbol.hasGetter;
	         g = f.symbol.getter(c.symbol);
	         s = f.symbol.setter(c.symbol);
	         if g.isPublic && !(f.symbol.name startsWith "$"))  // inserting $outer breaks the bean
        fieldList = javaName(f.symbol) :: javaName(g) :: (if (s != NoSymbol) javaName(s) else null) :: fieldList
      val methodList =
	     for (m <- clasz.methods
	         if !m.symbol.isConstructor &&
	         m.symbol.isPublic &&
	         !(m.symbol.name startsWith "$") &&
	         !m.symbol.isGetter &&
	         !m.symbol.isSetter) yield javaName(m.symbol)

      val constructor = beanInfoClass.addNewMethod(ACC_PUBLIC, "<init>", JType.VOID, new Array[JType](0), new Array[String](0))
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

      // push the string array of field information
      jcode emitPUSH fieldList.length
      jcode emitANEWARRAY strKind
      push(fieldList)

      // push the string array of method information
      jcode emitPUSH methodList.length
      jcode emitANEWARRAY strKind
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      jcode.emitINVOKESPECIAL("scala/beans/ScalaBeanInfo", "<init>", conType)
      jcode.emitRETURN()

      // write the bean information class file.
      writeClass("BeanInfo ", beanInfoClass.getName(), toByteArray(beanInfoClass), c.symbol)
    }

    /** Add the given 'throws' attributes to jmethod */
    def addExceptionsAttribute(jmethod: JMethod, excs: List[AnnotationInfo]) {
      if (excs.isEmpty) return

      val cpool = jmethod.getConstantPool
      val buf: ByteBuffer = ByteBuffer.allocate(512)
      var nattr = 0

      // put some random value; the actual number is determined at the end
      buf putShort 0xbaba.toShort

      for (ThrownException(exc) <- excs.distinct) {
        buf.putShort(
          cpool.addClass(
            javaName(exc)).shortValue)
        nattr += 1
      }

      assert(nattr > 0, nattr)
      buf.putShort(0, nattr.toShort)
      addAttribute(jmethod, tpnme.ExceptionsATTR, buf)
    }

    /** Whether an annotation should be emitted as a Java annotation
     *   .initialize: if 'annot' is read from pickle, atp might be un-initialized
     */
    private def shouldEmitAnnotation(annot: AnnotationInfo) =
      annot.symbol.initialize.isJavaDefined &&
      annot.matches(ClassfileAnnotationClass) &&
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
            case ClazzTag   =>
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
        assert(args.isEmpty, args)
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
    private def needsGenericSignature(sym: Symbol) = !(
      // PP: This condition used to include sym.hasExpandedName, but this leads
      // to the total loss of generic information if a private member is
      // accessed from a closure: both the field and the accessor were generated
      // without it.  This is particularly bad because the availability of
      // generic information could disappear as a consequence of a seemingly
      // unrelated change.
         settings.Ynogenericsig.value
      || sym.isArtifact
      || sym.isLiftedMethod
      || sym.isBridge
      || (sym.ownerChain exists (_.isImplClass))
    )
    def addGenericSignature(jmember: JMember, sym: Symbol, owner: Symbol) {
      if (needsGenericSignature(sym)) {
        val memberTpe = beforeErasure(owner.thisType.memberInfo(sym))

        erasure.javaSig(sym, memberTpe) foreach { sig =>
          // This seems useful enough in the general case.
          log(sig)
          if (checkSignatures) {
            val normalizedTpe = beforeErasure(erasure.prepareSigMap(memberTpe))
            val bytecodeTpe = owner.thisType.memberInfo(sym)
            if (!sym.isType && !sym.isConstructor && !(erasure.erasure(sym)(normalizedTpe) =:= bytecodeTpe)) {
              clasz.cunit.warning(sym.pos,
                  """|compiler bug: created generic signature for %s in %s that does not conform to its erasure
                     |signature: %s
                     |original type: %s
                     |normalized type: %s
                     |erasure type: %s
                     |if this is reproducible, please report bug at https://issues.scala-lang.org/
                  """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig, memberTpe, normalizedTpe, bytecodeTpe))
               return
            }
          }
          val index = jmember.getConstantPool.addUtf8(sig).toShort
          if (opt.verboseDebug)
            beforeErasure(println("add generic sig "+sym+":"+sym.info+" ==> "+sig+" @ "+index))

          val buf = ByteBuffer.allocate(2)
          buf putShort index
          addAttribute(jmember, tpnme.SignatureATTR, buf)
        }
      }
    }

    def addAnnotations(jmember: JMember, annotations: List[AnnotationInfo]) {
      if (annotations exists (_ matches definitions.DeprecatedAttr)) {
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
      /** The outer name for this inner class. Note that it returns null
       *  when the inner class should not get an index in the constant pool.
       *  That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
       */
      def outerName(innerSym: Symbol): String = {
        if (innerSym.originalEnclosingMethod != NoSymbol)
          null
        else {
          val outerName = javaName(innerSym.rawowner)
          if (isTopLevelModule(innerSym.rawowner)) "" + nme.stripModuleSuffix(newTermName(outerName))
          else outerName
        }
      }

      def innerName(innerSym: Symbol): String =
        if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction)
          null
        else
          innerSym.rawname + innerSym.moduleSuffix

      // add inner classes which might not have been referenced yet
      afterErasure {
        for (sym <- List(clasz.symbol, clasz.symbol.linkedClassOfClass); m <- sym.info.decls.map(innerClassSymbolFor) if m.isClass)
          innerClassBuffer += m
      }

      val allInners = innerClassBuffer.toList
      if (allInners.nonEmpty) {
        debuglog(clasz.symbol.fullName('.') + " contains " + allInners.size + " inner classes.")
        val innerClassesAttr = jclass.getInnerClasses()
        // sort them so inner classes succeed their enclosing class
        // to satisfy the Eclipse Java compiler
        for (innerSym <- allInners sortBy (_.name.length)) {
          val flags = {
            val staticFlag = if (innerSym.rawowner.hasModuleFlag) ACC_STATIC else 0
            (javaFlags(innerSym) | staticFlag) & INNER_CLASSES_FLAGS
          }
          val jname = javaName(innerSym)
          val oname = outerName(innerSym)
          val iname = innerName(innerSym)

          // Mimicking javap inner class output
          debuglog(
            if (oname == null || iname == null) "//class " + jname
            else "//%s=class %s of class %s".format(iname, jname, oname)
          )

          innerClassesAttr.addEntry(jname, oname, iname, flags)
        }
      }
    }

    def genField(f: IField) {
      debuglog("Adding field: " + f.symbol.fullName)

      val jfield = jclass.addNewField(
        javaFieldFlags(f.symbol),
        javaName(f.symbol),
        javaType(f.symbol.tpe)
      )

      addGenericSignature(jfield, f.symbol, clasz.symbol)
      addAnnotations(jfield, f.symbol.annotations)
    }

    def genMethod(m: IMethod) {
      if (m.symbol.isStaticConstructor || definitions.isGetClass(m.symbol)) return

      debuglog("Generating method " + m.symbol.fullName)
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
                                    mkArray(m.params map (p => javaType(p.kind))),
                                    mkArray(m.params map (p => javaName(p.sym))))

      addRemoteException(jmethod, m.symbol)

      if (!jmethod.isAbstract() && !method.native) {
        val jcode = jmethod.getCode().asInstanceOf[JExtendedCode]

        // add a fake local for debugging purposes
        if (emitVars && isClosureApply(method.symbol)) {
          val outerField = clasz.symbol.info.decl(nme.OUTER_LOCAL)
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this = new Local(
              method.symbol.newVariable(nme.FAKE_LOCAL_THIS), toTypeKind(outerField.tpe), false)
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
          debuglog("add local var: " + local)
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym))
        }

        genCode(m)
        if (emitVars)
          genLocalVariableTable(m, jcode)
      }

      addGenericSignature(jmethod, m.symbol, clasz.symbol)
      val (excs, others) = m.symbol.annotations partition (_.symbol == ThrowsClass)
      addExceptionsAttribute(jmethod, excs)
      addAnnotations(jmethod, others)
      addParamAnnotations(jmethod, m.params.map(_.sym.annotations))

      // check for code size
      try jmethod.freeze()
      catch {
        case e: JCode.CodeSizeTooBigException =>
          clasz.cunit.error(m.symbol.pos, "Code size exceeds JVM limits: %d".format(e.codeSize))
          throw e
      }
    }

    /** Adds a @remote annotation, actual use unknown.
     */
    private def addRemoteException(jmethod: JMethod, meth: Symbol) {
      val needsAnnotation = (
        (isRemoteClass || (meth hasAnnotation RemoteAttr) && jmethod.isPublic)
          && !(meth.throwsAnnotations contains RemoteExceptionClass)
      )
      if (needsAnnotation) {
        val c   = Constant(RemoteExceptionClass.tpe)
        val arg = Literal(c) setType c.tpe
        meth.addAnnotation(appliedType(ThrowsClass, c.tpe), arg)
      }
    }

    private def isClosureApply(sym: Symbol): Boolean = {
      (sym.name == nme.apply) &&
      sym.owner.isSynthetic &&
      sym.owner.tpe.parents.exists { t =>
        val TypeRef(_, sym, _) = t
        FunctionClass contains sym
      }
    }

    def addModuleInstanceField() {
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
          val oldLastBlock = m.lastBlock
          val lastBlock = m.newBlock()
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
            val fieldSymbol = clasz.symbol.newValue(newTermName(fieldName), NoPosition, STATIC | FINAL) setInfo LongClass.tpe
            clasz addField new IField(fieldSymbol)
            lastBlock emit CONSTANT(Constant(value))
            lastBlock emit STORE_FIELD(fieldSymbol, true)
          }

          if (isParcelableClass)
            addCreatorCode(BytecodeGenerator.this, lastBlock)

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

      clinit.emitRETURN()
    }

    /** Add a forwarder for method m */
    def addForwarder(jclass: JClass, module: Symbol, m: Symbol) {
      val moduleName     = javaName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes = methodInfo.paramTypes map javaType
      val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)
      // TODO: evaluate the other flags we might be dropping on the floor here.
      val flags = PublicStatic | (
        if (m.isVarargsMethod) ACC_VARARGS else 0
      )

      /** Forwarders must not be marked final, as the JVM will not allow
       *  redefinition of a final static method, and we don't know what classes
       *  might be subclassing the companion class.  See SI-4827.
       */
      val mirrorMethod = jclass.addNewMethod(
        flags,
        javaName(m),
        javaType(methodInfo.resultType),
        mkArray(paramJavaTypes),
        mkArray(paramNames))
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

      val (throws, others) = m.annotations partition (_.symbol == ThrowsClass)
      addExceptionsAttribute(mirrorMethod, throws)
      addAnnotations(mirrorMethod, others)
      addParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))
    }

    /** Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion
     *  object: method signature is not taken into account.
     */
    def addForwarders(jclass: JClass, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog("Dumping mirror class for object: " + moduleClass)

      val className    = jclass.getName
      val linkedClass  = moduleClass.companionClass
      val linkedModule = linkedClass.companionSymbol
      lazy val conflictingNames: Set[Name] = {
        linkedClass.info.members collect { case sym if sym.name.isTermName => sym.name } toSet
      }
      debuglog("Potentially conflicting names for forwarders: " + conflictingNames)

      for (m <- moduleClass.info.membersBasedOnFlags(ExcludedForwarderFlags, Flags.METHOD)) {
        if (m.isType || m.isDeferred || (m.owner eq ObjectClass) || m.isConstructor)
          debuglog("No forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
        else if (conflictingNames(m.name))
          log("No forwarder for " + m + " due to conflict with " + linkedClass.info.member(m.name))
        else {
          log("Adding static forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
          addForwarder(jclass, moduleClass, m)
        }
      }
    }

    /** Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     */
    def generateMirrorClass(clasz: Symbol, sourceFile: SourceFile) {
      import JAccessFlags._
      /* We need to save inner classes buffer and create a new one to make sure
       * that we do confuse inner classes of the class  we mirror with inner
       * classes of the class we are mirroring. These two sets can be different
       * as seen in this case:
       *
       *  class A {
       *   class B
       *   def b: B = new B
       *  }
       *  object C extends A
       *
       *  Here mirror class of C has a static forwarder for (inherited) method `b`
       *  therefore it refers to class `B` and needs InnerClasses entry. However,
       *  the real class for `C` (named `C$`) is empty and does not refer to `B`
       *  thus does not need InnerClasses entry it.
       *
       *  NOTE: This logic has been refactored in GenASM and everything is
       *  implemented in a much cleaner way by having two separate buffers.
       */
      val savedInnerClasses = innerClassBuffer
      innerClassBuffer = mutable.LinkedHashSet[Symbol]()
      val moduleName = javaName(clasz) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           JAVA_LANG_OBJECT.getName,
                                           JClass.NO_INTERFACES,
                                           "" + sourceFile)

      log("Dumping mirror class for '%s'".format(mirrorClass.getName))
      addForwarders(mirrorClass, clasz)
      val ssa = scalaSignatureAddingMarker(mirrorClass, clasz.companionSymbol)
      addAnnotations(mirrorClass, clasz.annotations ++ ssa)
      emitClass(mirrorClass, clasz)
      innerClassBuffer = savedInnerClasses
    }

    var linearization: List[BasicBlock] = Nil
    var isModuleInitialized = false

    /**
     *  @param m ...
     */
    def genCode(m: IMethod) {
      val jcode = jmethod.getCode.asInstanceOf[JExtendedCode]

      def makeLabels(bs: List[BasicBlock]) = {
        debuglog("Making labels for: " + method)

        mutable.HashMap(bs map (_ -> jcode.newLabel) : _*)
      }

      isModuleInitialized = false

      linearization = linearizer.linearize(m)
      val labels = makeLabels(linearization)

      var nextBlock: BasicBlock = linearization.head

      def genBlocks(l: List[BasicBlock]): Unit = l match {
        case Nil => ()
        case x :: Nil => nextBlock = null; genBlock(x)
        case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys)
      }

      /** Generate exception handlers for the current method. */
      def genExceptionHandlers() {

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
                ranges ::= ((start, end))
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
            ranges ::= ((start, jcode.getPC()))
          }

          if (!covered.isEmpty)
            debuglog("Some covered blocks were not found in method: " + method +
                  " covered: " + covered + " not in " + linearization)
          ranges
        }

        for (e <- this.method.exh ; p <- ranges(e).sortBy(_._1)) {
          if (p._1 < p._2) {
            debuglog("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
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

      def isAccessibleFrom(target: Symbol, site: Symbol): Boolean = {
        target.isPublic || target.isProtected && {
          (site.enclClass isSubClass target.enclClass) ||
          (site.enclosingPackage == target.privateWithin)
        }
      }

      def genCallMethod(call: CALL_METHOD) {
        val CALL_METHOD(method, style) = call
        val siteSymbol  = clasz.symbol
        val hostSymbol  = call.hostClass
        val methodOwner = method.owner
        // info calls so that types are up to date; erasure may add lateINTERFACE to traits
        hostSymbol.info ; methodOwner.info

        def isInterfaceCall(sym: Symbol) = (
             sym.isInterface && methodOwner != ObjectClass
          || sym.isJavaDefined && sym.isNonBottomSubClass(ClassfileAnnotationClass)
        )
        // whether to reference the type of the receiver or
        // the type of the method owner (if not an interface!)
        val useMethodOwner = (
             style != Dynamic
          || !isInterfaceCall(hostSymbol) && isAccessibleFrom(methodOwner, siteSymbol)
          || hostSymbol.isBottomClass
        )
        val receiver = if (useMethodOwner) methodOwner else hostSymbol
        val jowner   = javaName(receiver)
        val jname    = javaName(method)
        val jtype    = javaType(method).asInstanceOf[JMethodType]

        def dbg(invoke: String) {
          debuglog("%s %s %s.%s:%s".format(invoke, receiver.accessString, jowner, jname, jtype))
        }

        def initModule() {
          // we initialize the MODULE$ field immediately after the super ctor
          if (isStaticModule(siteSymbol) && !isModuleInitialized &&
              jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
              jname == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
            isModuleInitialized = true
            jcode.emitALOAD_0()
            jcode.emitPUTSTATIC(jclass.getName(),
                                nme.MODULE_INSTANCE_FIELD.toString,
                                jclass.getType())
          }
        }

        style match {
          case Static(true)                         => dbg("invokespecial");    jcode.emitINVOKESPECIAL(jowner, jname, jtype)
          case Static(false)                        => dbg("invokestatic");      jcode.emitINVOKESTATIC(jowner, jname, jtype)
          case Dynamic if isInterfaceCall(receiver) => dbg("invokinterface"); jcode.emitINVOKEINTERFACE(jowner, jname, jtype)
          case Dynamic                              => dbg("invokevirtual");    jcode.emitINVOKEVIRTUAL(jowner, jname, jtype)
          case SuperCall(_)                         =>
            dbg("invokespecial")
            jcode.emitINVOKESPECIAL(jowner, jname, jtype)
            initModule()
        }
      }

      def genBlock(b: BasicBlock) {
        labels(b).anchorToNext()

        debuglog("Generating code for block: " + b + " at pc: " + labels(b).getAnchor())
        var lastMappedPC = 0
        var lastLineNr = 0
        var crtPC = 0

        /** local variables whose scope appears in this block. */
        val varsInBlock: mutable.Set[Local] = new mutable.HashSet
        val lastInstr = b.lastInstruction

        for (instr <- b) {
          instr match {
            case THIS(clasz)           => jcode.emitALOAD_0()

            case CONSTANT(const)       => genConstant(jcode, const)

            case LOAD_ARRAY_ITEM(kind) =>
              if(kind.isRefOrArrayType) { jcode.emitAALOAD() }
              else {
                (kind: @unchecked) match {
                  case UNIT            => throw new IllegalArgumentException("invalid type for aload " + kind)
                  case BOOL | BYTE     => jcode.emitBALOAD()
                  case SHORT           => jcode.emitSALOAD()
                  case CHAR            => jcode.emitCALOAD()
                  case INT             => jcode.emitIALOAD()
                  case LONG            => jcode.emitLALOAD()
                  case FLOAT           => jcode.emitFALOAD()
                  case DOUBLE          => jcode.emitDALOAD()
                }
              }

            case LOAD_LOCAL(local)     => jcode.emitLOAD(indexOf(local), javaType(local.kind))

            case lf @ LOAD_FIELD(field, isStatic) =>
              var owner = javaName(lf.hostClass)
              debuglog("LOAD_FIELD with owner: " + owner +
                    " flags: " + Flags.flagsToString(field.owner.flags))
              val fieldJName = javaName(field)
              val fieldJType = javaType(field)
              if (isStatic) jcode.emitGETSTATIC(owner, fieldJName, fieldJType)
              else          jcode.emitGETFIELD( owner, fieldJName, fieldJType)

            case LOAD_MODULE(module) =>
              // assert(module.isModule, "Expected module: " + module)
              debuglog("generating LOAD_MODULE for: " + module + " flags: " + Flags.flagsToString(module.flags));
              if (clasz.symbol == module.moduleClass && jmethod.getName() != nme.readResolve.toString)
                jcode.emitALOAD_0()
              else
                jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                    nme.MODULE_INSTANCE_FIELD.toString,
                                    javaType(module))

            case STORE_ARRAY_ITEM(kind) =>
              if(kind.isRefOrArrayType) { jcode.emitAASTORE() }
              else {
                (kind: @unchecked) match {
                  case UNIT            => throw new IllegalArgumentException("invalid type for astore " + kind)
                  case BOOL | BYTE     => jcode.emitBASTORE()
                  case SHORT           => jcode.emitSASTORE()
                  case CHAR            => jcode.emitCASTORE()
                  case INT             => jcode.emitIASTORE()
                  case LONG            => jcode.emitLASTORE()
                  case FLOAT           => jcode.emitFASTORE()
                  case DOUBLE          => jcode.emitDASTORE()
                }
              }

            case STORE_LOCAL(local) =>
              jcode.emitSTORE(indexOf(local), javaType(local.kind))

            case STORE_THIS(_) =>
              // this only works for impl classes because the self parameter comes first
              // in the method signature. If that changes, this code has to be revisited.
              jcode.emitASTORE_0()

            case STORE_FIELD(field, isStatic) =>
              val owner = javaName(field.owner)
              val fieldJName = javaName(field)
              val fieldJType = javaType(field)
              if (isStatic) jcode.emitPUTSTATIC(owner, fieldJName, fieldJType)
              else          jcode.emitPUTFIELD( owner, fieldJName, fieldJType)

            case CALL_PRIMITIVE(primitive) => genPrimitive(primitive, instr.pos)

            /** Special handling to access native Array.clone() */
            case call @ CALL_METHOD(definitions.Array_clone, Dynamic) =>
              val target: String = javaType(call.targetTypeKind).getSignature()
              jcode.emitINVOKEVIRTUAL(target, "clone", arrayCloneType)

            case call @ CALL_METHOD(method, style) => genCallMethod(call)

            case BOX(kind) =>
              val Pair(mname, mtype) = jBoxTo(kind)
              jcode.emitINVOKESTATIC(BoxesRunTime, mname, mtype)

            case UNBOX(kind) =>
              val Pair(mname, mtype) = jUnboxTo(kind)
              jcode.emitINVOKESTATIC(BoxesRunTime, mname, mtype)

            case NEW(REFERENCE(cls)) =>
              val className = javaName(cls)
              jcode emitNEW className

            case CREATE_ARRAY(elem, 1) =>
              if(elem.isRefOrArrayType) { jcode emitANEWARRAY javaType(elem).asInstanceOf[JReferenceType] }
              else                      { jcode emitNEWARRAY  javaType(elem) }

            case CREATE_ARRAY(elem, dims) =>
              jcode.emitMULTIANEWARRAY(javaType(ArrayN(elem, dims)).asInstanceOf[JReferenceType], dims)

            case IS_INSTANCE(tpe) =>
              tpe match {
                case REFERENCE(cls) => jcode emitINSTANCEOF new JObjectType(javaName(cls))
                case ARRAY(elem)    => jcode emitINSTANCEOF new JArrayType(javaType(elem))
                case _              => abort("Unknown reference type in IS_INSTANCE: " + tpe)
              }

            case CHECK_CAST(tpe) =>
              tpe match {
                case REFERENCE(cls) => if (cls != ObjectClass) { jcode emitCHECKCAST new JObjectType(javaName(cls)) } // No need to checkcast for Objects
                case ARRAY(elem)    => jcode emitCHECKCAST new JArrayType(javaType(elem))
                case _              => abort("Unknown reference type in IS_INSTANCE: " + tpe)
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
              debuglog("Emitting SWITCH:\ntags: " + tags + "\nbranches: " + branches)
              jcode.emitSWITCH(tagArray,
                               branchArray,
                               labels(branches.last),
                               MIN_SWITCH_DENSITY)
              ()

            case JUMP(whereto) =>
              if (nextBlock != whereto)
                jcode.emitGOTO_maybe_W(labels(whereto), false) // default to short jumps

            case CJUMP(success, failure, cond, kind) =>
              if(kind.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(conds(cond.negate()), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
              } else if(kind.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(cond.negate()), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
              } else {
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
                  jcode.emitIF(conds(cond.negate()), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
              }

            case CZJUMP(success, failure, cond, kind) =>
              if(kind.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
                if (nextBlock == success) {
                  jcode.emitIF(conds(cond.negate()), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
              } else if(kind.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
                val Success = success
                val Failure = failure
                (cond, nextBlock) match {
                  case (EQ, Success) => jcode emitIFNONNULL labels(failure)
                  case (NE, Failure) => jcode emitIFNONNULL labels(success)
                  case (EQ, Failure) => jcode emitIFNULL    labels(success)
                  case (NE, Success) => jcode emitIFNULL    labels(failure)
                  case (EQ, _) =>
                    jcode emitIFNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                  case (NE, _) =>
                    jcode emitIFNONNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                  case _ =>
                }
              } else {
                (kind: @unchecked) match {
                  case LONG   =>
                    jcode.emitLCONST_0()
                    jcode.emitLCMP()
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
                  jcode.emitIF(conds(cond.negate()), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
              }

            case RETURN(kind) => jcode emitRETURN javaType(kind)

            case THROW(_)     => jcode.emitATHROW()

            case DROP(kind) =>
              if(kind.isWideType) jcode.emitPOP2()
              else                jcode.emitPOP()

            case DUP(kind) =>
              if(kind.isWideType) jcode.emitDUP2()
              else                jcode.emitDUP()

            case MONITOR_ENTER() => jcode.emitMONITORENTER()

            case MONITOR_EXIT()  => jcode.emitMONITOREXIT()

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
              }
              else dumpMethodAndAbort(method, "Illegal local var nesting")

            case LOAD_EXCEPTION(_) =>
              ()
          }

          crtPC = jcode.getPC()

          // assert(instr.pos.source.isEmpty || instr.pos.source.get == (clasz.cunit.source), "sources don't match")
          // val crtLine = instr.pos.line.get(lastLineNr);

          val crtLine = try {
            if (instr.pos == NoPosition) lastLineNr else (instr.pos).line // check NoPosition to avoid costly exception
          } catch {
            case _: UnsupportedOperationException =>
              log("Warning: wrong position in: " + method)
              lastLineNr
          }

          if (instr eq lastInstr) { endPC(b) = jcode.getPC() }

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
            if(kind.isIntSizedType) { jcode.emitINEG() }
            else {
              kind match {
                case LONG   => jcode.emitLNEG()
                case FLOAT  => jcode.emitFNEG()
                case DOUBLE => jcode.emitDNEG()
                case _ => abort("Impossible to negate a " + kind)
              }
            }

          case Arithmetic(op, kind) =>
            op match {
              case ADD =>
                if(kind.isIntSizedType) { jcode.emitIADD() }
                else {
                  (kind: @unchecked) match {
                    case LONG   => jcode.emitLADD()
                    case FLOAT  => jcode.emitFADD()
                    case DOUBLE => jcode.emitDADD()
                  }
                }

              case SUB =>
                if(kind.isIntSizedType) { jcode.emitISUB() }
                else {
                  (kind: @unchecked) match {
                    case LONG   => jcode.emitLSUB()
                    case FLOAT  => jcode.emitFSUB()
                    case DOUBLE => jcode.emitDSUB()
                  }
                }

              case MUL =>
                if(kind.isIntSizedType) { jcode.emitIMUL() }
                else {
                  (kind: @unchecked) match {
                    case LONG   => jcode.emitLMUL()
                    case FLOAT  => jcode.emitFMUL()
                    case DOUBLE => jcode.emitDMUL()
                  }
                }

              case DIV =>
                if(kind.isIntSizedType) { jcode.emitIDIV() }
                else {
                  (kind: @unchecked) match {
                    case LONG   => jcode.emitLDIV()
                    case FLOAT  => jcode.emitFDIV()
                    case DOUBLE => jcode.emitDDIV()
                  }
                }

              case REM =>
                if(kind.isIntSizedType) { jcode.emitIREM() }
                else {
                  (kind: @unchecked) match {
                    case LONG   => jcode.emitLREM()
                    case FLOAT  => jcode.emitFREM()
                    case DOUBLE => jcode.emitDREM()
                  }
                }

              case NOT =>
                if(kind.isIntSizedType) {
                  jcode.emitPUSH(-1)
                  jcode.emitIXOR()
                } else if(kind == LONG) {
                  jcode.emitPUSH(-1l)
                  jcode.emitLXOR()
                } else {
                  abort("Impossible to negate an " + kind)
                }

              case _ =>
                abort("Unknown arithmetic primitive " + primitive)
            }

          case Logical(op, kind) => ((op, kind): @unchecked) match {
            case (AND, LONG) => jcode.emitLAND()
            case (AND, INT)  => jcode.emitIAND()
            case (AND, _)    =>
              jcode.emitIAND()
              if (kind != BOOL)
                jcode.emitT2T(javaType(INT), javaType(kind));

            case (OR, LONG) => jcode.emitLOR()
            case (OR, INT)  => jcode.emitIOR()
            case (OR, _) =>
              jcode.emitIOR()
              if (kind != BOOL)
                jcode.emitT2T(javaType(INT), javaType(kind));

            case (XOR, LONG) => jcode.emitLXOR()
            case (XOR, INT)  => jcode.emitIXOR()
            case (XOR, _) =>
              jcode.emitIXOR()
              if (kind != BOOL)
                jcode.emitT2T(javaType(INT), javaType(kind));
          }

          case Shift(op, kind) => ((op, kind): @unchecked) match {
            case (LSL, LONG) => jcode.emitLSHL()
            case (LSL, INT)  => jcode.emitISHL()
            case (LSL, _) =>
              jcode.emitISHL()
              jcode.emitT2T(javaType(INT), javaType(kind))

            case (ASR, LONG) => jcode.emitLSHR()
            case (ASR, INT)  => jcode.emitISHR()
            case (ASR, _) =>
              jcode.emitISHR()
              jcode.emitT2T(javaType(INT), javaType(kind))

            case (LSR, LONG) => jcode.emitLUSHR()
            case (LSR, INT)  => jcode.emitIUSHR()
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
            debuglog("Converting from: " + src + " to: " + dst)
            if (dst == BOOL) {
              println("Illegal conversion at: " + clasz + " at: " + pos.source + ":" + pos.line)
            } else
              jcode.emitT2T(javaType(src), javaType(dst))

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
        lvTab putShort start
        lvTab putShort end
        lvTab putShort pool.addUtf8(name).toShort
        lvTab putShort pool.addUtf8(signature).toShort
        lvTab putShort idx
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
                                    jcode,
                                    tpnme.LocalVariableTableATTR.toString,
                                    lvTab.array())
      jcode addAttribute attr
    }


    /** For each basic block, the first PC address following it. */
    val endPC = new mutable.HashMap[BasicBlock, Int]

    ////////////////////// local vars ///////////////////////

    def sizeOf(sym: Symbol): Int = sizeOf(toTypeKind(sym.tpe))

    def sizeOf(k: TypeKind): Int = if(k.isWideType) 2 else 1

    def indexOf(m: IMethod, sym: Symbol): Int = {
      val Some(local) = m lookupLocal sym
      indexOf(local)
    }

    def indexOf(local: Local): Int = {
      assert(local.index >= 0, "Invalid index for: " + local + "{" + local.## + "}: ")
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given
     * method. *Does not assume the parameters come first!*
     */
    def computeLocalVarsIndex(m: IMethod) {
      var idx = if (m.symbol.isStaticMember) 0 else 1;

      for (l <- m.params) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }

      for (l <- m.locals if !(m.params contains l)) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }
    }

    ////////////////////// Utilities ////////////////////////

    /** Merge adjacent ranges. */
    private def mergeEntries(ranges: List[(Int, Int)]): List[(Int, Int)] =
      (ranges.foldLeft(Nil: List[(Int, Int)]) { (collapsed: List[(Int, Int)], p: (Int, Int)) => (collapsed, p) match {
        case (Nil, _) => List(p)
        case ((s1, e1) :: rest, (s2, e2)) if (e1 == s2) => (s1, e2) :: rest
        case _ => p :: collapsed
      }}).reverse
  }

  private def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

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
    // constructors of module classes should be private
    // PP: why are they only being marked private at this stage and not earlier?
    val privateFlag =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModule(sym.owner))

    // Final: the only fields which can receive ACC_FINAL are eager vals.
    // Neither vars nor lazy vals can, because:
    //
    // Source: http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
    // "Another problem is that the specification allows aggressive
    // optimization of final fields. Within a thread, it is permissible to
    // reorder reads of a final field with those modifications of a final
    // field that do not take place in the constructor."
    //
    // A var or lazy val which is marked final still has meaning to the
    // scala compiler.  The word final is heavily overloaded unfortunately;
    // for us it means "not overridable".  At present you can't override
    // vars regardless; this may change.
    //
    // The logic does not check .isFinal (which checks flags for the FINAL flag,
    // and includes symbols marked lateFINAL) instead inspecting rawflags so
    // we can exclude lateFINAL.  Such symbols are eligible for inlining, but to
    // avoid breaking proxy software which depends on subclassing, we do not
    // emit ACC_FINAL.
    // Nested objects won't receive ACC_FINAL in order to allow for their overriding.

    val finalFlag = (
         (((sym.rawflags & Flags.FINAL) != 0) || isTopLevelModule(sym))
      && !sym.enclClass.isInterface
      && !sym.isClassConstructor
      && !sym.isMutable   // lazy vals and vars both
    )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract if present.
    mkFlags(
      if (privateFlag) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (finalFlag && !sym.hasAbstractFlag) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge) ACC_BRIDGE | ACC_SYNTHETIC else 0,
      if (sym.isArtifact) ACC_SYNTHETIC else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0,
      if (sym.hasFlag(Flags.SYNCHRONIZED)) JAVA_ACC_SYNCHRONIZED else 0
    )
  }
  def javaFieldFlags(sym: Symbol) = (
    javaFlags(sym) | mkFlags(
      if (sym hasAnnotation TransientAttr) ACC_TRANSIENT else 0,
      if (sym hasAnnotation VolatileAttr) ACC_VOLATILE else 0,
      if (sym.isMutable) 0 else ACC_FINAL
    )
  )

  def isTopLevelModule(sym: Symbol): Boolean =
    afterPickler { sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass }

  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

}
