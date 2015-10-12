/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.jvm

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.pickling.{ PickleFormat, PickleBuffer }
import scala.tools.nsc.backend.jvm.opt.InlineInfoAttribute
import scala.tools.nsc.symtab._
import scala.tools.asm
import asm.Label
import scala.annotation.tailrec

/**
 *  @author  Iulian Dragos (version 1.0, FJBG-based implementation)
 *  @author  Miguel Garcia (version 2.0,  ASM-based implementation)
 *
 * Documentation at http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/2012Q2/GenASM.pdf
 */
abstract class GenASM extends SubComponent with BytecodeWriters { self =>
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions._

  val bCodeAsmCommon: BCodeAsmCommon[global.type] = new BCodeAsmCommon(global)
  import bCodeAsmCommon._

  // Strangely I can't find this in the asm code
  // 255, but reserving 1 for "this"
  final val MaximumJvmParameters = 254

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase): Phase = new AsmPhase(p)

  /** From the reference documentation of the Android SDK:
   *  The `Parcelable` interface identifies classes whose instances can be written to and restored from a `Parcel`.
   *  Classes implementing the `Parcelable` interface must also have a static field called `CREATOR`,
   *  which is an object implementing the `Parcelable.Creator` interface.
   */
  private val androidFieldName = newTermName("CREATOR")

  private lazy val AndroidParcelableInterface = rootMirror.getClassIfDefined("android.os.Parcelable")
  private lazy val AndroidCreatorClass        = rootMirror.getClassIfDefined("android.os.Parcelable$Creator")

  /** JVM code generation phase
   */
  class AsmPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName
    override def erasedTypes = true
    def apply(cls: IClass) = sys.error("no implementation")

    // An AsmPhase starts and ends within a Run, thus the caches in question will get populated and cleared within a Run, too), SI-7422
    javaNameCache.clear()
    javaNameCache ++= List(
      NothingClass        -> binarynme.RuntimeNothing,
      RuntimeNothingClass -> binarynme.RuntimeNothing,
      NullClass           -> binarynme.RuntimeNull,
      RuntimeNullClass    -> binarynme.RuntimeNull
    )

    // unlike javaNameCache, reverseJavaName contains entries only for class symbols and their internal names.
    reverseJavaName.clear()
    reverseJavaName ++= List(
      binarynme.RuntimeNothing.toString() -> RuntimeNothingClass, // RuntimeNothingClass is the bytecode-level return type of Scala methods with Nothing return-type.
      binarynme.RuntimeNull.toString()    -> RuntimeNullClass
    )

    // Lazy val; can't have eager vals in Phase constructors which may
    // cause cycles before Global has finished initialization.
    lazy val BeanInfoAttr = rootMirror.getRequiredClass("scala.beans.BeanInfo")

    private def initBytecodeWriter(entryPoints: List[IClass]): BytecodeWriter = {
      settings.outputDirs.getSingleOutput match {
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

        case _ => factoryNonJarBytecodeWriter()
      }
    }

    private def isJavaEntryPoint(icls: IClass) = {
      val sym = icls.symbol
      def fail(msg: String, pos: Position = sym.pos) = {
        reporter.warning(sym.pos,
          sym.name + " has a main method with parameter type Array[String], but " + sym.fullName('.') + " will not be a runnable program.\n" +
            "  Reason: " + msg
          // TODO: make this next claim true, if possible
          //   by generating valid main methods as static in module classes
          //   not sure what the jvm allows here
          // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
        )
        false
      }
      def failNoForwarder(msg: String) = {
        fail(msg + ", which means no static forwarder can be generated.\n")
      }
      val possibles = if (sym.hasModuleFlag) (sym.tpe nonPrivateMember nme.main).alternatives else Nil
      val hasApproximate = possibles exists { m =>
        m.info match {
          case MethodType(p :: Nil, _) => p.tpe.typeSymbol == ArrayClass
          case _                       => false
        }
      }
      // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
      hasApproximate && {
        // Before erasure so we can identify generic mains.
        enteringErasure {
          val companion     = sym.linkedClassOfClass

          if (hasJavaMainMethod(companion))
            failNoForwarder("companion contains its own main method")
          else if (companion.tpe.member(nme.main) != NoSymbol)
            // this is only because forwarders aren't smart enough yet
            failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
          else if (companion.isTrait)
            failNoForwarder("companion is a trait")
          // Now either succeeed, or issue some additional warnings for things which look like
          // attempts to be java main methods.
          else (possibles exists isJavaMainMethod) || {
            possibles exists { m =>
              m.info match {
                case PolyType(_, _) =>
                  fail("main methods cannot be generic.")
                case MethodType(params, res) =>
                  if (res.typeSymbol :: params exists (_.isAbstractType))
                    fail("main methods cannot refer to type parameters or abstract types.", m.pos)
                  else
                    isJavaMainMethod(m) || fail("main method must have exact signature (Array[String])Unit", m.pos)
                case tp =>
                  fail("don't know what this is: " + tp, m.pos)
              }
            }
          }
        }
      }
    }

    override def run() {

      if (settings.debug)
        inform("[running phase " + name + " on icode]")

      if (settings.Xdce) {
        val classes = icodes.classes.keys.toList // copy to avoid mutating the map while iterating
        for (sym <- classes if inliner.isClosureClass(sym) && !deadCode.liveClosures(sym)) {
          log(s"Optimizer eliminated ${sym.fullNameString}")
          deadCode.elidedClosures += sym
          icodes.classes -= sym
        }
      }

      // For predictably ordered error messages.
      var sortedClasses = classes.values.toList sortBy (_.symbol.fullName)

      // Warn when classes will overwrite one another on case-insensitive systems.
      for ((_, v1 :: v2 :: _) <- sortedClasses groupBy (_.symbol.javaClassName.toString.toLowerCase)) {
        reporter.warning(v1.symbol.pos,
          s"Class ${v1.symbol.javaClassName} differs only in case from ${v2.symbol.javaClassName}. " +
          "Such classes will overwrite one another on case-insensitive filesystems.")
      }

      debuglog(s"Created new bytecode generator for ${classes.size} classes.")
      val bytecodeWriter  = initBytecodeWriter(sortedClasses filter isJavaEntryPoint)
      val needsOutfile    = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      val plainCodeGen    = new JPlainBuilder(   bytecodeWriter, needsOutfile)
      val mirrorCodeGen   = new JMirrorBuilder(  bytecodeWriter, needsOutfile)
      val beanInfoCodeGen = new JBeanInfoBuilder(bytecodeWriter, needsOutfile)

      def emitFor(c: IClass) {
        if (isStaticModule(c.symbol) && isTopLevelModule(c.symbol)) {
          if (c.symbol.companionClass == NoSymbol)
            mirrorCodeGen genMirrorClass (c.symbol, c.cunit)
          else
            log(s"No mirror class for module with linked class: ${c.symbol.fullName}")
        }
        plainCodeGen genClass c
        if (c.symbol hasAnnotation BeanInfoAttr) beanInfoCodeGen genBeanInfoClass c
      }

      while (!sortedClasses.isEmpty) {
        val c = sortedClasses.head
        try emitFor(c)
        catch {
          case e: FileConflictException =>
            reporter.error(c.symbol.pos, s"error writing ${c.symbol}: ${e.getMessage}")
        }
        sortedClasses = sortedClasses.tail
        classes -= c.symbol // GC opportunity
      }

      bytecodeWriter.close()

      /* don't javaNameCache.clear() because that causes the following tests to fail:
       *   test/files/run/macro-repl-dontexpand.scala
       *   test/files/jvm/interpreter.scala
       * TODO but why? what use could javaNameCache possibly see once GenASM is over?
       */

      /* TODO After emitting all class files (e.g., in a separate compiler phase) ASM can perform bytecode verification:
       *
       * (1) call the asm.util.CheckAdapter.verify() overload:
       *     public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *
       * (2) passing a custom ClassLoader to verify inter-dependent classes.
       *
       * Alternatively, an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       */

    } // end of AsmPhase.run()

  } // end of class AsmPhase

  var pickledBytes = 0 // statistics

  val javaNameCache = perRunCaches.newAnyRefMap[Symbol, Name]()

  // unlike javaNameCache, reverseJavaName contains entries only for class symbols and their internal names.
  val reverseJavaName = perRunCaches.newAnyRefMap[String, Symbol]()

  private def mkFlags(args: Int*)         = args.foldLeft(0)(_ | _)
  private def hasPublicBitSet(flags: Int) = (flags & asm.Opcodes.ACC_PUBLIC) != 0
  private def isRemote(s: Symbol)         = s hasAnnotation RemoteAttr

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
    // scala compiler. The word final is heavily overloaded unfortunately;
    // for us it means "not overridable". At present you can't override
    // vars regardless; this may change.
    //
    // The logic does not check .isFinal (which checks flags for the FINAL flag,
    // and includes symbols marked lateFINAL) instead inspecting rawflags so
    // we can exclude lateFINAL. Such symbols are eligible for inlining, but to
    // avoid breaking proxy software which depends on subclassing, we do not
    // emit ACC_FINAL.
    // Nested objects won't receive ACC_FINAL in order to allow for their overriding.

    val finalFlag = (
         (((sym.rawflags & Flags.FINAL) != 0) || isTopLevelModule(sym))
      && !sym.enclClass.isInterface
      && !sym.isClassConstructor
      && !sym.isMutable // lazy vals and vars both
    )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract if present.
    import asm.Opcodes._
    mkFlags(
      if (privateFlag) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (finalFlag && !sym.hasAbstractFlag) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge) ACC_BRIDGE | ACC_SYNTHETIC else 0,
      if (sym.isArtifact) ACC_SYNTHETIC else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.hasJavaEnumFlag) ACC_ENUM else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0,
      if (sym.hasFlag(Flags.SYNCHRONIZED)) ACC_SYNCHRONIZED else 0
    )
  }

  def javaFieldFlags(sym: Symbol) = {
    javaFlags(sym) | mkFlags(
      if (sym hasAnnotation TransientAttr) asm.Opcodes.ACC_TRANSIENT else 0,
      if (sym hasAnnotation VolatileAttr)  asm.Opcodes.ACC_VOLATILE  else 0,
      if (sym.isMutable) 0 else asm.Opcodes.ACC_FINAL
    )
  }

  def isTopLevelModule(sym: Symbol): Boolean =
    exitingPickler { sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass }

  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://issues.scala-lang.org/browse/SI-3872
  // -----------------------------------------------------------------------------------------

  /**
   * Given an internal name (eg "java/lang/Integer") returns the class symbol for it.
   *
   * Better not to need this method (an example where control flow arrives here is welcome).
   * This method is invoked only upon both (1) and (2) below happening:
   *   (1) providing an asm.ClassWriter with an internal name by other means than javaName()
   *   (2) forgetting to track the corresponding class-symbol in reverseJavaName.
   *
   * (The first item is already unlikely because we rely on javaName()
   *  to do the bookkeeping for entries that should go in innerClassBuffer.)
   *
   * (We could do completely without this method at the expense of computing stack-map-frames ourselves and
   *  invoking visitFrame(), but that would require another pass over all instructions.)
   *
   * Right now I can't think of any invocation of visitSomething() on MethodVisitor
   * where we hand an internal name not backed by a reverseJavaName.
   * However, I'm leaving this note just in case any such oversight is discovered.
   */
  def inameToSymbol(iname: String): Symbol = {
    val name = global.newTypeName(iname)
    val res0 =
      if (nme.isModuleName(name)) rootMirror.getModuleByName(name.dropModule)
      else                        rootMirror.getClassByName(name.replace('/', '.')) // TODO fails for inner classes (but this hasn't been tested).
    assert(res0 != NoSymbol)
    val res = jsymbol(res0)
    res
  }

  def jsymbol(sym: Symbol): Symbol = {
    if(sym.isJavaDefined && sym.isModuleClass) sym.linkedClassOfClass
    else if(sym.isModule) sym.moduleClass
    else sym // we track only module-classes and plain-classes
  }

  private def superClasses(s: Symbol): List[Symbol] = {
    assert(!s.isInterface)
    s.superClass match {
      case NoSymbol => List(s)
      case sc       => s :: superClasses(sc)
    }
  }

  private def firstCommonSuffix(as: List[Symbol], bs: List[Symbol]): Symbol = {
    assert(!(as contains NoSymbol))
    assert(!(bs contains NoSymbol))
    var chainA = as
    var chainB = bs
    var fcs: Symbol = NoSymbol
    do {
      if      (chainB contains chainA.head) fcs = chainA.head
      else if (chainA contains chainB.head) fcs = chainB.head
      else {
        chainA = chainA.tail
        chainB = chainB.tail
      }
    } while(fcs == NoSymbol)
    fcs
  }

  private def jvmWiseLUB(a: Symbol, b: Symbol): Symbol = {
    assert(a.isClass)
    assert(b.isClass)

    val res = (a.isInterface, b.isInterface) match {
      case (true, true) =>
        global.lub(List(a.tpe, b.tpe)).typeSymbol // TODO assert == firstCommonSuffix of resp. parents
      case (true, false) =>
        if(b isSubClass a) a else ObjectClass
      case (false, true) =>
        if(a isSubClass b) b else ObjectClass
      case _ =>
        firstCommonSuffix(superClasses(a), superClasses(b))
    }
    assert(res != NoSymbol)
    res
  }

  /* The internal name of the least common ancestor of the types given by inameA and inameB.
     It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow */
  def getCommonSuperClass(inameA: String, inameB: String): String = {
    val a = reverseJavaName.getOrElseUpdate(inameA, inameToSymbol(inameA))
    val b = reverseJavaName.getOrElseUpdate(inameB, inameToSymbol(inameB))

    // global.lub(List(a.tpe, b.tpe)).typeSymbol.javaBinaryName.toString()
    // icodes.lub(icodes.toTypeKind(a.tpe), icodes.toTypeKind(b.tpe)).toType
    val lcaSym  = jvmWiseLUB(a, b)
    val lcaName = lcaSym.javaBinaryName.toString // don't call javaName because that side-effects innerClassBuffer.
    val oldsym  = reverseJavaName.put(lcaName, lcaSym)
    assert(oldsym.isEmpty || (oldsym.get == lcaSym), "somehow we're not managing to compute common-super-class for ASM consumption")
    assert(lcaName != "scala/Any")

    lcaName // TODO ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Do some caching.
  }

  class CClassWriter(flags: Int) extends asm.ClassWriter(flags) {
    override def getCommonSuperClass(iname1: String, iname2: String): String = {
      GenASM.this.getCommonSuperClass(iname1, iname2)
    }
  }

  // -----------------------------------------------------------------------------------------
  // constants
  // -----------------------------------------------------------------------------------------

  private val classfileVersion: Int = settings.target.value match {
    case "jvm-1.5"     => asm.Opcodes.V1_5
    case "jvm-1.6"     => asm.Opcodes.V1_6
    case "jvm-1.7"     => asm.Opcodes.V1_7
    case "jvm-1.8"     => asm.Opcodes.V1_8
  }

  private val majorVersion: Int = (classfileVersion & 0xFF)
  private val emitStackMapFrame = (majorVersion >= 50)

  private val extraProc: Int = mkFlags(
    asm.ClassWriter.COMPUTE_MAXS,
    if(emitStackMapFrame) asm.ClassWriter.COMPUTE_FRAMES else 0
  )

  val JAVA_LANG_OBJECT = asm.Type.getObjectType("java/lang/Object")
  val JAVA_LANG_STRING = asm.Type.getObjectType("java/lang/String")

  /**
   *  We call many Java varargs methods from ASM library that expect Arra[asm.Type] as argument so
   *  we override default (compiler-generated) ClassTag so we can provide specialized newArray implementation.
   *
   *  Examples of methods that should pick our definition are: JBuilder.javaType and JPlainBuilder.genMethod.
   */
  private implicit val asmTypeTag: scala.reflect.ClassTag[asm.Type] = new scala.reflect.ClassTag[asm.Type] {
    def runtimeClass: java.lang.Class[asm.Type] = classOf[asm.Type]
    final override def newArray(len: Int): Array[asm.Type] = new Array[asm.Type](len)
  }

  /** basic functionality for class file building */
  abstract class JBuilder(bytecodeWriter: BytecodeWriter, needsOutfile: Boolean) {

    val EMPTY_STRING_ARRAY = Array.empty[String]

    val mdesc_arglessvoid = "()V"

    val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
    val INSTANCE_CONSTRUCTOR_NAME = "<init>"

    // -----------------------------------------------------------------------------------------
    // factory methods
    // -----------------------------------------------------------------------------------------

    /**
     * Returns a new ClassWriter for the class given by arguments.
     *
     * @param access the class's access flags. This parameter also indicates if the class is deprecated.
     *
     * @param name the internal name of the class.
     *
     * @param signature the signature of this class. May be <tt>null</tt> if
     *        the class is not a generic one, and does not extend or implement
     *        generic classes or interfaces.
     *
     * @param superName the internal of name of the super class. For interfaces,
     *        the super class is [[Object]]. May be <tt>null</tt>, but
     *        only for the [[Object]] class.
     *
     * @param interfaces the internal names of the class's interfaces (see
     *        {@link Type#getInternalName() getInternalName}). May be
     *        <tt>null</tt>.
     */
    def createJClass(access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): asm.ClassWriter = {
      val cw = new CClassWriter(extraProc)
      cw.visit(classfileVersion,
               access, name, signature,
               superName, interfaces)

      cw
    }

    def createJAttribute(name: String, b: Array[Byte], offset: Int, len: Int): asm.Attribute = {
      val dest = new Array[Byte](len)
      System.arraycopy(b, offset, dest, 0, len)
      new asm.CustomAttr(name, dest)
    }

    // -----------------------------------------------------------------------------------------
    // utilities useful when emitting plain, mirror, and beaninfo classes.
    // -----------------------------------------------------------------------------------------

    def writeIfNotTooBig(label: String, jclassName: String, jclass: asm.ClassWriter, sym: Symbol) {
      try {
        val arr = jclass.toByteArray()
        val outF: scala.tools.nsc.io.AbstractFile = {
          if(needsOutfile) getFile(sym, jclassName, ".class") else null
        }
        bytecodeWriter.writeClass(label, jclassName, arr, outF)
      } catch {
        case e: java.lang.RuntimeException if e.getMessage != null && (e.getMessage contains "too large!") =>
          reporter.error(sym.pos,
            s"Could not write class $jclassName because it exceeds JVM code size limits. ${e.getMessage}")
        case e: java.io.IOException if e.getMessage != null && (e.getMessage contains "File name too long")  =>
          reporter.error(sym.pos, e.getMessage + "\n" +
            "This can happen on some encrypted or legacy file systems.  Please see SI-3623 for more details.")

      }
    }

    /** Specialized array conversion to prevent calling
     *  java.lang.reflect.Array.newInstance via TraversableOnce.toArray
     */
    def mkArray(xs: Traversable[String]):    Array[String]    = { val a = new Array[String](xs.size);   xs.copyToArray(a); a }

    // -----------------------------------------------------------------------------------------
    // Getters for (JVMS 4.2) internal and unqualified names (represented as JType instances).
    // These getters track behind the scenes the inner classes referred to in the class being emitted,
    // so as to build the InnerClasses attribute (JVMS 4.7.6) via `addInnerClasses()`
    // (which also adds as member classes those inner classes that have been declared,
    // thus also covering the case of inner classes declared but otherwise not referred).
    // -----------------------------------------------------------------------------------------

    val innerClassBuffer = mutable.LinkedHashSet[Symbol]()

    /** For given symbol return a symbol corresponding to a class that should be declared as inner class.
     *
     *  For example:
     *  class A {
     *    class B
     *    object C
     *  }
     *
     *  then method will return:
     *    NoSymbol for A,
     *    the same symbol for A.B (corresponding to A$B class), and
     *    A$C$ symbol for A.C.
     */
    def innerClassSymbolFor(s: Symbol): Symbol =
      if (s.isClass) s else if (s.isModule) s.moduleClass else NoSymbol

    /** Return the name of this symbol that can be used on the Java platform.  It removes spaces from names.
     *
     *  Special handling:
     *    scala.Nothing erases to scala.runtime.Nothing$
     *       scala.Null erases to scala.runtime.Null$
     *
     *  This is needed because they are not real classes, and they mean
     *  'abrupt termination upon evaluation of that expression' or null respectively.
     *  This handling is done already in GenICode, but here we need to remove
     *  references from method signatures to these types, because such classes
     *  cannot exist in the classpath: the type checker will be very confused.
     */
    def javaName(sym: Symbol): String = {

        /*
         * Checks if given symbol corresponds to inner class/object and add it to innerClassBuffer
         *
         * Note: This method is called recursively thus making sure that we add complete chain
         * of inner class all until root class.
         */
        def collectInnerClass(s: Symbol): Unit = {
          // TODO: some enteringFlatten { ... } which accounts for
          // being nested in parameterized classes (if we're going to selectively flatten.)
          val x = innerClassSymbolFor(s)
          if(x ne NoSymbol) {
            assert(x.isClass, "not an inner-class symbol")
            // impl classes are considered top-level, see comment in BTypes
            val isInner = !considerAsTopLevelImplementationArtifact(s) && !x.rawowner.isPackageClass
            if (isInner) {
              innerClassBuffer += x
              collectInnerClass(x.rawowner)
            }
          }
        }

      collectInnerClass(sym)

      val hasInternalName = sym.isClass || sym.isModuleNotMethod
      val cachedJN = javaNameCache.getOrElseUpdate(sym, {
        if (hasInternalName) { sym.javaBinaryName }
        else                 { sym.javaSimpleName }
      })

      if(emitStackMapFrame && hasInternalName) {
        val internalName = cachedJN.toString()
        val trackedSym = jsymbol(sym)
        reverseJavaName.get(internalName) match {
          case None =>
            reverseJavaName.put(internalName, trackedSym)
          case Some(oldsym) =>
            // TODO: `duplicateOk` seems pretty ad-hoc (a more aggressive version caused SI-9356 because it called oldSym.exists, which failed in the unpickler; see also SI-5031)
            def duplicateOk = oldsym == NoSymbol || trackedSym == NoSymbol || (syntheticCoreClasses contains oldsym) || (oldsym.isModuleClass && (oldsym.sourceModule == trackedSym.sourceModule))
            if (oldsym != trackedSym && !duplicateOk)
              devWarning(s"""|Different class symbols have the same bytecode-level internal name:
                             |     name: $internalName
                             |   oldsym: ${oldsym.fullNameString}
                             |  tracked: ${trackedSym.fullNameString}""".stripMargin)
        }
      }

      cachedJN.toString
    }

    def descriptor(t: Type):     String = { javaType(t).getDescriptor }
    def descriptor(k: TypeKind): String = { javaType(k).getDescriptor }
    def descriptor(s: Symbol):   String = { javaType(s).getDescriptor }

    def javaType(tk: TypeKind): asm.Type = {
      if(tk.isValueType) {
        if(tk.isIntSizedType) {
          (tk: @unchecked) match {
            case BOOL   => asm.Type.BOOLEAN_TYPE
            case BYTE   => asm.Type.BYTE_TYPE
            case SHORT  => asm.Type.SHORT_TYPE
            case CHAR   => asm.Type.CHAR_TYPE
            case INT    => asm.Type.INT_TYPE
          }
        } else {
          (tk: @unchecked) match {
            case UNIT   => asm.Type.VOID_TYPE
            case LONG   => asm.Type.LONG_TYPE
            case FLOAT  => asm.Type.FLOAT_TYPE
            case DOUBLE => asm.Type.DOUBLE_TYPE
          }
        }
      } else {
        assert(!tk.isBoxedType, tk) // documentation (BOXED matches none below anyway)
        (tk: @unchecked) match {
          case REFERENCE(cls)  => asm.Type.getObjectType(javaName(cls))
          case ARRAY(elem)     => javaArrayType(javaType(elem))
        }
      }
    }

    def javaType(t: Type): asm.Type = javaType(toTypeKind(t))

    def javaType(s: Symbol): asm.Type = {
      if (s.isMethod) {
        val resT: asm.Type = if (s.isClassConstructor) asm.Type.VOID_TYPE else javaType(s.tpe.resultType)
        asm.Type.getMethodType( resT, (s.tpe.paramTypes map javaType): _*)
      } else { javaType(s.tpe) }
    }

    def javaArrayType(elem: asm.Type): asm.Type = { asm.Type.getObjectType("[" + elem.getDescriptor) }

    def isDeprecated(sym: Symbol): Boolean = { sym.annotations exists (_ matches definitions.DeprecatedAttr) }

    def addInnerClasses(csym: Symbol, jclass: asm.ClassVisitor, isMirror: Boolean = false) {
      /* The outer name for this inner class. Note that it returns null
       * when the inner class should not get an index in the constant pool.
       * That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
       */
      def outerName(innerSym: Symbol): String = {
        if (isAnonymousOrLocalClass(innerSym))
          null
        else {
          val outerName = javaName(innerSym.rawowner)
          if (isTopLevelModule(innerSym.rawowner)) "" + TermName(outerName).dropModule
          else outerName
        }
      }

      def innerName(innerSym: Symbol): String = {
        // phase travel necessary: after flatten, the name includes the name of outer classes.
        // if some outer name contains $anon, a non-anon class is considered anon.
        if (exitingPickler(innerSym.isAnonymousClass || innerSym.isAnonymousFunction)) null
        else innerSym.rawname + innerSym.moduleSuffix
      }

      val linkedClass = exitingPickler(csym.linkedClassOfClass) // linkedCoC does not work properly in late phases

      innerClassBuffer ++= {
        val members = exitingPickler(memberClassesForInnerClassTable(csym))
        // lambdalift makes all classes (also local, anonymous) members of their enclosing class
        val allNested = exitingPhase(currentRun.lambdaliftPhase)(memberClassesForInnerClassTable(csym))
        val nested = {
          // Classes nested in value classes are nested in the companion at this point. For InnerClass /
          // EnclosingMethod, we use the value class as the outer class. So we remove nested classes
          // from the companion that were originally nested in the value class.
          if (exitingPickler(linkedClass.isDerivedValueClass)) allNested.filterNot(classOriginallyNestedInClass(_, linkedClass))
          else allNested
        }

        // for the mirror class, we take the members of the companion module class (Java compat, see doc in BTypes.scala).
        // for module classes, we filter out those members.
        if (isMirror) members
        else if (isTopLevelModule(csym)) nested diff members
        else nested
      }

      if (!considerAsTopLevelImplementationArtifact(csym)) {
        // If this is a top-level non-impl class, add members of the companion object. These are the
        // classes for which we change the InnerClass entry to allow using them from Java.
        // We exclude impl classes: if the classfile for the impl class exists on the classpath, a
        // linkedClass symbol is found for which isTopLevelModule is true, so we end up searching
        // members of that weird impl-class-module-class-symbol. that search probably cannot return
        // any classes, but it's better to exclude it.
        if (linkedClass != NoSymbol && isTopLevelModule(linkedClass)) {
          // phase travel to exitingPickler: this makes sure that memberClassesForInnerClassTable only
          // sees member classes, not local classes that were lifted by lambdalift.
          innerClassBuffer ++= exitingPickler(memberClassesForInnerClassTable(linkedClass))
        }

        // Classes nested in value classes are nested in the companion at this point. For InnerClass /
        // EnclosingMethod we use the value class as enclosing class. Here we search nested classes
        // in the companion that were originally nested in the value class, and we add them as nested
        // in the value class.
        if (linkedClass != NoSymbol && exitingPickler(csym.isDerivedValueClass)) {
          val moduleMemberClasses = exitingPhase(currentRun.lambdaliftPhase)(memberClassesForInnerClassTable(linkedClass))
          innerClassBuffer ++= moduleMemberClasses.filter(classOriginallyNestedInClass(_, csym))
        }
      }

      val allInners: List[Symbol] = innerClassBuffer.toList filterNot deadCode.elidedClosures

      if (allInners.nonEmpty) {
        debuglog(csym.fullName('.') + " contains " + allInners.size + " inner classes.")

        // entries ready to be serialized into the classfile, used to detect duplicates.
        val entries = mutable.Map.empty[String, String]

        // sort them so inner classes succeed their enclosing class to satisfy the Eclipse Java compiler
        for (innerSym <- allInners sortBy (_.name.length)) { // TODO why not sortBy (_.name.toString()) ??
          val flagsWithFinal: Int = mkFlags(
            // See comment in BTypes, when is a class marked static in the InnerClass table.
            if (isOriginallyStaticOwner(innerSym.originalOwner)) asm.Opcodes.ACC_STATIC else 0,
            (if (innerSym.isJava) javaClassfileFlags(innerSym) else javaFlags(innerSym)) & ~asm.Opcodes.ACC_STATIC,
            if(isDeprecated(innerSym)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo-access flag
          ) & (BCodeAsmCommon.INNER_CLASSES_FLAGS | asm.Opcodes.ACC_DEPRECATED)
          val flags = if (innerSym.isModuleClass) flagsWithFinal & ~asm.Opcodes.ACC_FINAL else flagsWithFinal // For SI-5676, object overriding.
          val jname = javaName(innerSym)  // never null
          val oname = outerName(innerSym) // null when method-enclosed
          val iname = innerName(innerSym) // null for anonymous inner class

          // Mimicking javap inner class output
          debuglog(
            if (oname == null || iname == null) "//class " + jname
            else "//%s=class %s of class %s".format(iname, jname, oname)
          )

          assert(jname != null, "javaName is broken.") // documentation
          val doAdd = entries.get(jname) match {
            // TODO is it ok for prevOName to be null? (Someone should really document the invariants of the InnerClasses bytecode attribute)
            case Some(prevOName) =>
              // this occurs e.g. when innerClassBuffer contains both class Thread$State, object Thread$State,
              // i.e. for them it must be the case that oname == java/lang/Thread
              assert(prevOName == oname, "duplicate")
              false
            case None => true
          }

          if(doAdd) {
            entries += (jname -> oname)
            jclass.visitInnerClass(jname, oname, iname, flags)
          }

          /*
           * TODO assert (JVMS 4.7.6 The InnerClasses attribute)
           * If a class file has a version number that is greater than or equal to 51.0, and
           * has an InnerClasses attribute in its attributes table, then for all entries in the
           * classes array of the InnerClasses attribute, the value of the
           * outer_class_info_index item must be zero if the value of the
           * inner_name_index item is zero.
           */

        }
      }
    }

  } // end of class JBuilder


  /** functionality for building plain and mirror classes */
  abstract class JCommonBuilder(bytecodeWriter: BytecodeWriter, needsOutfile: Boolean) extends JBuilder(bytecodeWriter, needsOutfile) {

    def debugLevel = settings.debuginfo.indexOfChoice

    val emitSource = debugLevel >= 1
    val emitLines  = debugLevel >= 2
    val emitVars   = debugLevel >= 3

    // -----------------------------------------------------------------------------------------
    // more constants
    // -----------------------------------------------------------------------------------------

    val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
    val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

    val strMODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString

    // -----------------------------------------------------------------------------------------
    // Custom attribute (JVMS 4.7.1) "ScalaSig" used as marker only
    // i.e., the pickle is contained in a custom annotation, see:
    //   (1) `addAnnotations()`,
    //   (2) SID # 10 (draft) - Storage of pickled Scala signatures in class files, http://www.scala-lang.org/sid/10
    //   (3) SID # 5 - Internals of Scala Annotations, http://www.scala-lang.org/sid/5
    // That annotation in turn is not related to the "java-generic-signature" (JVMS 4.7.9)
    // other than both ending up encoded as attributes (JVMS 4.7)
    // (with the caveat that the "ScalaSig" attribute is associated to some classes,
    // while the "Signature" attribute can be associated to classes, methods, and fields.)
    // -----------------------------------------------------------------------------------------

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    def pickleMarkerLocal = {
      createJAttribute(tpnme.ScalaSignatureATTR.toString, versionPickle.bytes, 0, versionPickle.writeIndex)
    }

    def pickleMarkerForeign = {
      createJAttribute(tpnme.ScalaATTR.toString, new Array[Byte](0), 0, 0)
    }

    /** Returns a ScalaSignature annotation if it must be added to this class, none otherwise.
     *  This annotation must be added to the class' annotations list when generating them.
     *
     *  Depending on whether the returned option is defined, it adds to `jclass` one of:
     *    (a) the ScalaSig marker attribute
     *        (indicating that a scala-signature-annotation aka pickle is present in this class); or
     *    (b) the Scala marker attribute
     *        (indicating that a scala-signature-annotation aka pickle is to be found in another file).
     *
     *
     *  @param jclassName The class file that is being readied.
     *  @param sym    The symbol for which the signature has been entered in the symData map.
     *                This is different than the symbol
     *                that is being generated in the case of a mirror class.
     *  @return       An option that is:
     *                - defined and contains an AnnotationInfo of the ScalaSignature type,
     *                  instantiated with the pickle signature for sym.
     *                - empty if the jclass/sym pair must not contain a pickle.
     *
     */
    def getAnnotPickle(jclassName: String, sym: Symbol): Option[AnnotationInfo] = {
      currentRun.symData get sym match {
        case Some(pickle) if !nme.isModuleName(newTermName(jclassName)) =>
          val scalaAnnot = {
            val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
            AnnotationInfo(sigBytes.sigAnnot, Nil, List((nme.bytes, sigBytes)))
          }
          pickledBytes += pickle.writeIndex
          currentRun.symData -= sym
          currentRun.symData -= sym.companionSymbol
          Some(scalaAnnot)
        case _ =>
          None
      }
    }

    /**
     * Quoting from JVMS 4.7.5 The Exceptions Attribute
     *   "The Exceptions attribute indicates which checked exceptions a method may throw.
     *    There may be at most one Exceptions attribute in each method_info structure."
     *
     * The contents of that attribute are determined by the `String[] exceptions` argument to ASM's ClassVisitor.visitMethod()
     * This method returns such list of internal names.
     */
    def getExceptions(excs: List[AnnotationInfo]): List[String] =
      for (ThrownException(exc) <- excs.distinct)
      yield javaName(exc)

    def getCurrentCUnit(): CompilationUnit

    def getGenericSignature(sym: Symbol, owner: Symbol) = self.getGenericSignature(sym, owner, getCurrentCUnit())

    def emitArgument(av:   asm.AnnotationVisitor,
                     name: String,
                     arg:  ClassfileAnnotArg) {
      (arg: @unchecked) match {

        case LiteralAnnotArg(const) =>
          if(const.isNonUnitAnyVal) { av.visit(name, const.value) }
          else {
            const.tag match {
              case StringTag  =>
                assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
                av.visit(name, const.stringValue)  // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
              case ClazzTag   => av.visit(name, javaType(const.typeValue))
              case EnumTag =>
                val edesc  = descriptor(const.tpe) // the class descriptor of the enumeration class.
                val evalue = const.symbolValue.name.toString // value the actual enumeration value.
                av.visitEnum(name, edesc, evalue)
            }
          }

        case sb@ScalaSigBytes(bytes) =>
          // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
          // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
          if (sb.fitsInOneString)
            av.visit(name, strEncode(sb))
          else {
            val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
            for(arg <- arrEncode(sb)) { arrAnnotV.visit(name, arg) }
            arrAnnotV.visitEnd()
          }
          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.

        case ArrayAnnotArg(args) =>
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- args) { emitArgument(arrAnnotV, null, arg) }
          arrAnnotV.visitEnd()

        case NestedAnnotArg(annInfo) =>
          val AnnotationInfo(typ, args, assocs) = annInfo
          assert(args.isEmpty, args)
          val desc = descriptor(typ) // the class descriptor of the nested annotation class
          val nestedVisitor = av.visitAnnotation(name, desc)
          emitAssocs(nestedVisitor, assocs)
      }
    }

    def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, ClassfileAnnotArg)]) {
      for ((name, value) <- assocs) {
        emitArgument(av, name.toString(), value)
      }
      av.visitEnd()
    }

    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = cw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = mw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = fw.visitAnnotation(descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }
    }

    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[AnnotationInfo]]) {
      val annotationss = pannotss map (_ filter shouldEmitAnnotation)
      if (annotationss forall (_.isEmpty)) return
      for ((annots, idx) <- annotationss.zipWithIndex;
           annot <- annots) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, descriptor(typ), isRuntimeVisible(annot))
        emitAssocs(pannVisitor, assocs)
      }
    }

    /** Adds a @remote annotation, actual use unknown.
     *
     * Invoked from genMethod() and addForwarder().
     */
    def addRemoteExceptionAnnot(isRemoteClass: Boolean, isJMethodPublic: Boolean, meth: Symbol) {
      val needsAnnotation = (
        (  isRemoteClass ||
           isRemote(meth) && isJMethodPublic
        ) && !(meth.throwsAnnotations contains RemoteExceptionClass)
      )
      if (needsAnnotation) {
        val c   = Constant(RemoteExceptionClass.tpe)
        val arg = Literal(c) setType c.tpe
        meth.addAnnotation(appliedType(ThrowsClass, c.tpe), arg)
      }
    }

    // -----------------------------------------------------------------------------------------
    // Static forwarders (related to mirror classes but also present in
    // a plain class lacking companion module, for details see `isCandidateForForwarders`).
    // -----------------------------------------------------------------------------------------

    /** Add a forwarder for method m. Used only from addForwarders(). */
    private def addForwarder(isRemoteClass: Boolean, jclass: asm.ClassVisitor, module: Symbol, m: Symbol) {
      val moduleName     = javaName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[asm.Type] = methodInfo.paramTypes map javaType
      // val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)

      /* Forwarders must not be marked final,
       * as the JVM will not allow redefinition of a final static method,
       * and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      // TODO: ACC_SYNTHETIC ?
      val flags = PublicStatic | (
        if (m.isVarargsMethod) asm.Opcodes.ACC_VARARGS else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jgensig = staticForwarderGenericSignature(m, module, getCurrentCUnit())
      addRemoteExceptionAnnot(isRemoteClass, hasPublicBitSet(flags), m)
      val (throws, others) = m.annotations partition (_.symbol == ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(throws)

      val jReturnType = javaType(methodInfo.resultType)
      val mdesc = asm.Type.getMethodDescriptor(jReturnType, paramJavaTypes: _*)
      val mirrorMethodName = javaName(m)
      val mirrorMethod: asm.MethodVisitor = jclass.visitMethod(
        flags,
        mirrorMethodName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      )

      // typestate: entering mode with valid call sequences:
      //   [ visitAnnotationDefault ] ( visitAnnotation | visitParameterAnnotation | visitAttribute )*

      emitAnnotations(mirrorMethod, others)
      emitParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))

      // typestate: entering mode with valid call sequences:
      //   visitCode ( visitFrame | visitXInsn | visitLabel | visitTryCatchBlock | visitLocalVariable | visitLineNumber )* visitMaxs ] visitEnd

      mirrorMethod.visitCode()

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, strMODULE_INSTANCE_FIELD, descriptor(module))

      var index = 0
      for(jparamType <- paramJavaTypes) {
        mirrorMethod.visitVarInsn(jparamType.getOpcode(asm.Opcodes.ILOAD), index)
        assert(jparamType.getSort() != asm.Type.METHOD, jparamType)
        index += jparamType.getSize()
      }

      mirrorMethod.visitMethodInsn(asm.Opcodes.INVOKEVIRTUAL, moduleName, mirrorMethodName, javaType(m).getDescriptor, false)
      mirrorMethod.visitInsn(jReturnType.getOpcode(asm.Opcodes.IRETURN))

      mirrorMethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      mirrorMethod.visitEnd()

    }

    /** Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion object:
     *  method signature is not taken into account.
     */
    def addForwarders(isRemoteClass: Boolean, jclass: asm.ClassVisitor, jclassName: String, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog("Dumping mirror class for object: " + moduleClass)

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] = {
        (linkedClass.info.members collect { case sym if sym.name.isTermName => sym.name }).toSet
      }
      debuglog("Potentially conflicting names for forwarders: " + conflictingNames)

      for (m <- moduleClass.info.membersBasedOnFlags(ExcludedForwarderFlags, Flags.METHOD)) {
        if (m.isType || m.isDeferred || (m.owner eq ObjectClass) || m.isConstructor)
          debuglog(s"No forwarder for '$m' from $jclassName to '$moduleClass'")
        else if (conflictingNames(m.name))
          log(s"No forwarder for $m due to conflict with " + linkedClass.info.member(m.name))
        else if (m.hasAccessBoundary)
          log(s"No forwarder for non-public member $m")
        else {
          debuglog(s"Adding static forwarder for '$m' from $jclassName to '$moduleClass'")
          addForwarder(isRemoteClass, jclass, moduleClass, m)
        }
      }
    }

  } // end of class JCommonBuilder


  trait JAndroidBuilder {
    self: JPlainBuilder =>

    def isAndroidParcelableClass(sym: Symbol) =
      (AndroidParcelableInterface != NoSymbol) &&
      (sym.parentSymbols contains AndroidParcelableInterface)

    /* Typestate: should be called before emitting fields (because it adds an IField to the current IClass). */
    def addCreatorCode(block: BasicBlock) {
      val fieldSymbol = (
        clasz.symbol.newValue(androidFieldName, NoPosition, Flags.STATIC | Flags.FINAL)
          setInfo AndroidCreatorClass.tpe
      )
      val methodSymbol = definitions.getMember(clasz.symbol.companionModule, androidFieldName)
      clasz addField new IField(fieldSymbol)
      block emit CALL_METHOD(methodSymbol, Static(onInstance = false))
      block emit STORE_FIELD(fieldSymbol, isStatic = true)
    }

    def legacyAddCreatorCode(clinit: asm.MethodVisitor) {
      val creatorType: asm.Type = javaType(AndroidCreatorClass)
      val tdesc_creator = creatorType.getDescriptor

      jclass.visitField(
        PublicStaticFinal,
        androidFieldName.toString,
        tdesc_creator,
        null, // no java-generic-signature
        null  // no initial value
      ).visitEnd()

      val moduleName = javaName(clasz.symbol)+"$"

      // GETSTATIC `moduleName`.MODULE$ : `moduleName`;
      clinit.visitFieldInsn(
        asm.Opcodes.GETSTATIC,
        moduleName,
        strMODULE_INSTANCE_FIELD,
        asm.Type.getObjectType(moduleName).getDescriptor
      )

      // INVOKEVIRTUAL `moduleName`.CREATOR() : android.os.Parcelable$Creator;
      clinit.visitMethodInsn(
        asm.Opcodes.INVOKEVIRTUAL,
        moduleName,
        androidFieldName.toString,
        asm.Type.getMethodDescriptor(creatorType, Array.empty[asm.Type]: _*),
        false
      )

      // PUTSTATIC `thisName`.CREATOR;
      clinit.visitFieldInsn(
        asm.Opcodes.PUTSTATIC,
        thisName,
        androidFieldName.toString,
        tdesc_creator
      )
    }

  } // end of trait JAndroidBuilder

  /** Map from type kinds to the Java reference types.
   *  It is used to push class literals onto the operand stack.
   *  @see Predef.classOf
   *  @see genConstant()
   */
  private val classLiteral = immutable.Map[TypeKind, asm.Type](
    UNIT   -> asm.Type.getObjectType("java/lang/Void"),
    BOOL   -> asm.Type.getObjectType("java/lang/Boolean"),
    BYTE   -> asm.Type.getObjectType("java/lang/Byte"),
    SHORT  -> asm.Type.getObjectType("java/lang/Short"),
    CHAR   -> asm.Type.getObjectType("java/lang/Character"),
    INT    -> asm.Type.getObjectType("java/lang/Integer"),
    LONG   -> asm.Type.getObjectType("java/lang/Long"),
    FLOAT  -> asm.Type.getObjectType("java/lang/Float"),
    DOUBLE -> asm.Type.getObjectType("java/lang/Double")
  )

  def isNonUnitValueTK(tk: TypeKind): Boolean = { tk.isValueType && tk != UNIT }

  case class MethodNameAndType(mname: String, mdesc: String)

  private val jBoxTo: Map[TypeKind, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("boxToBoolean",   "(Z)Ljava/lang/Boolean;"  ) ,
      BYTE   -> MethodNameAndType("boxToByte",      "(B)Ljava/lang/Byte;"     ) ,
      CHAR   -> MethodNameAndType("boxToCharacter", "(C)Ljava/lang/Character;") ,
      SHORT  -> MethodNameAndType("boxToShort",     "(S)Ljava/lang/Short;"    ) ,
      INT    -> MethodNameAndType("boxToInteger",   "(I)Ljava/lang/Integer;"  ) ,
      LONG   -> MethodNameAndType("boxToLong",      "(J)Ljava/lang/Long;"     ) ,
      FLOAT  -> MethodNameAndType("boxToFloat",     "(F)Ljava/lang/Float;"    ) ,
      DOUBLE -> MethodNameAndType("boxToDouble",    "(D)Ljava/lang/Double;"   )
    )
  }

  private val jUnboxTo: Map[TypeKind, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("unboxToBoolean", "(Ljava/lang/Object;)Z") ,
      BYTE   -> MethodNameAndType("unboxToByte",    "(Ljava/lang/Object;)B") ,
      CHAR   -> MethodNameAndType("unboxToChar",    "(Ljava/lang/Object;)C") ,
      SHORT  -> MethodNameAndType("unboxToShort",   "(Ljava/lang/Object;)S") ,
      INT    -> MethodNameAndType("unboxToInt",     "(Ljava/lang/Object;)I") ,
      LONG   -> MethodNameAndType("unboxToLong",    "(Ljava/lang/Object;)J") ,
      FLOAT  -> MethodNameAndType("unboxToFloat",   "(Ljava/lang/Object;)F") ,
      DOUBLE -> MethodNameAndType("unboxToDouble",  "(Ljava/lang/Object;)D")
    )
  }

  case class BlockInteval(start: BasicBlock, end: BasicBlock)

  /** builder of plain classes */
  class JPlainBuilder(bytecodeWriter: BytecodeWriter, needsOutfile: Boolean)
    extends JCommonBuilder(bytecodeWriter, needsOutfile)
    with    JAndroidBuilder {

    val MIN_SWITCH_DENSITY = 0.7

    val StringBuilderClassName = javaName(definitions.StringBuilderClass)
    val BoxesRunTime = "scala/runtime/BoxesRunTime"

    val StringBuilderType = asm.Type.getObjectType(StringBuilderClassName)
    val mdesc_toString    = "()Ljava/lang/String;"
    val mdesc_arrayClone  = "()Ljava/lang/Object;"

    val tdesc_long        = asm.Type.LONG_TYPE.getDescriptor // ie. "J"

    def isParcelableClass = isAndroidParcelableClass(clasz.symbol)

    def serialVUID: Option[Long] = genBCode.serialVUID(clasz.symbol)

    var clasz:    IClass = _           // this var must be assigned only by genClass()
    var jclass:   asm.ClassWriter = _  // the classfile being emitted
    var thisName: String = _           // the internal name of jclass

    def thisDescr: String = {
      assert(thisName != null, "thisDescr invoked too soon.")
      asm.Type.getObjectType(thisName).getDescriptor
    }

    def getCurrentCUnit(): CompilationUnit = { clasz.cunit }

    def genClass(c: IClass) {
      clasz = c
      innerClassBuffer.clear()

      thisName = javaName(c.symbol) // the internal name of the class being emitted

      val ps = c.symbol.info.parents
      val superClass: String = if(ps.isEmpty) JAVA_LANG_OBJECT.getInternalName else javaName(ps.head.typeSymbol)

      val ifaces: Array[String] = implementedInterfaces(c.symbol).map(javaName)(collection.breakOut)

      val thisSignature = getGenericSignature(c.symbol, c.symbol.owner)
      val flags = mkFlags(
        javaFlags(c.symbol),
        if(isDeprecated(c.symbol)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
      )
      jclass  = createJClass(flags,
                             thisName, thisSignature,
                             superClass, ifaces)

      // typestate: entering mode with valid call sequences:
      //   [ visitSource ] [ visitOuterClass ] ( visitAnnotation | visitAttribute )*

      if(emitSource) {
        jclass.visitSource(c.cunit.source.toString,
                           null /* SourceDebugExtension */)
      }

      enclosingMethodAttribute(clasz.symbol, javaName, javaType(_).getDescriptor) match {
        case Some(EnclosingMethodEntry(className, methodName, methodDescriptor)) =>
          jclass.visitOuterClass(className, methodName, methodDescriptor)
        case _ => ()
      }

      // typestate: entering mode with valid call sequences:
      //   ( visitAnnotation | visitAttribute )*

      val ssa = getAnnotPickle(thisName, c.symbol)
      jclass.visitAttribute(if(ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(jclass, c.symbol.annotations ++ ssa)

      if (!settings.YskipInlineInfoAttribute.value)
        jclass.visitAttribute(InlineInfoAttribute(buildInlineInfoFromClassSymbol(c.symbol, javaName, javaType(_).getDescriptor)))

      // typestate: entering mode with valid call sequences:
      //   ( visitInnerClass | visitField | visitMethod )* visitEnd

      if (isStaticModule(c.symbol) || isParcelableClass) {

        if (isStaticModule(c.symbol)) { addModuleInstanceField() }
        addStaticInit(c.lookupStaticCtor)

      } else {

        for (constructor <- c.lookupStaticCtor) {
          addStaticInit(Some(constructor))
        }
        val skipStaticForwarders = (c.symbol.isInterface || settings.noForwarders)
        if (!skipStaticForwarders) {
          val lmoc = c.symbol.companionModule
          // add static forwarders if there are no name conflicts; see bugs #363 and #1735
          if (lmoc != NoSymbol) {
            // it must be a top level class (name contains no $s)
            val isCandidateForForwarders = {
              exitingPickler { !(lmoc.name.toString contains '$') && lmoc.hasModuleFlag && !lmoc.isImplClass && !lmoc.isNestedClass }
            }
            if (isCandidateForForwarders) {
              log("Adding static forwarders from '%s' to implementations in '%s'".format(c.symbol, lmoc))
              addForwarders(isRemote(clasz.symbol), jclass, thisName, lmoc.moduleClass)
            }
          }
        }

      }

      // add static serialVersionUID field if `clasz` annotated with `@SerialVersionUID(uid: Long)`
      serialVUID foreach { value =>
        val fieldName = "serialVersionUID"
        jclass.visitField(
          PublicStaticFinal,
          fieldName,
          tdesc_long,
          null, // no java-generic-signature
          value
        ).visitEnd()
      }

      clasz.fields  foreach genField
      clasz.methods foreach { im => genMethod(im, c.symbol.isInterface) }

      addInnerClasses(clasz.symbol, jclass)
      jclass.visitEnd()
      writeIfNotTooBig("" + c.symbol.name, thisName, jclass, c.symbol)
    }

    def genField(f: IField) {
      debuglog("Adding field: " + f.symbol.fullName)

      val javagensig = getGenericSignature(f.symbol, clasz.symbol)

      val flags = mkFlags(
        javaFieldFlags(f.symbol),
        if(isDeprecated(f.symbol)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
      )

      val jfield: asm.FieldVisitor = jclass.visitField(
        flags,
        javaName(f.symbol),
        javaType(f.symbol.tpe).getDescriptor(),
        javagensig,
        null // no initial value
      )

      emitAnnotations(jfield, f.symbol.annotations)
      jfield.visitEnd()
    }

    var method:  IMethod = _
    var jmethod: asm.MethodVisitor = _
    var jMethodName: String = _

    final def emit(opc: Int) { jmethod.visitInsn(opc) }

    def genMethod(m: IMethod, isJInterface: Boolean) {

        def isClosureApply(sym: Symbol): Boolean = {
          (sym.name == nme.apply) &&
          sym.owner.isSynthetic &&
          sym.owner.tpe.parents.exists { t =>
            val TypeRef(_, sym, _) = t
            FunctionClass.seq contains sym
          }
        }

      if (m.symbol.isStaticConstructor || definitions.isGetClass(m.symbol)) return

      if (m.params.size > MaximumJvmParameters) {
        reporter.error(m.symbol.pos, s"Platform restriction: a parameter list's length cannot exceed $MaximumJvmParameters.")
        return
      }

      debuglog("Generating method " + m.symbol.fullName)
      method = m
      computeLocalVarsIndex(m)

      var resTpe: asm.Type = javaType(m.symbol.tpe.resultType)
      if (m.symbol.isClassConstructor)
        resTpe = asm.Type.VOID_TYPE

      val flags = mkFlags(
        javaFlags(m.symbol),
        if (isJInterface)          asm.Opcodes.ACC_ABSTRACT   else 0,
        if (m.symbol.isStrictFP)   asm.Opcodes.ACC_STRICT     else 0,
        if (method.native)         asm.Opcodes.ACC_NATIVE     else 0, // native methods of objects are generated in mirror classes
        if(isDeprecated(m.symbol)) asm.Opcodes.ACC_DEPRECATED else 0  // ASM pseudo access flag
      )

      // TODO needed? for(ann <- m.symbol.annotations) { ann.symbol.initialize }
      val jgensig = getGenericSignature(m.symbol, clasz.symbol)
      addRemoteExceptionAnnot(isRemote(clasz.symbol), hasPublicBitSet(flags), m.symbol)
      val (excs, others) = m.symbol.annotations partition (_.symbol == ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(excs)

      jMethodName = javaName(m.symbol)
      val mdesc = asm.Type.getMethodDescriptor(resTpe, (m.params map (p => javaType(p.kind))): _*)
      jmethod = jclass.visitMethod(
        flags,
        jMethodName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      )

      // TODO param names: (m.params map (p => javaName(p.sym)))

      // typestate: entering mode with valid call sequences: (see ASM Guide, 3.2.1)
      //   [ visitAnnotationDefault ] ( visitAnnotation | visitParameterAnnotation | visitAttribute )*

      emitAnnotations(jmethod, others)
      emitParamAnnotations(jmethod, m.params.map(_.sym.annotations))

      // typestate: entering mode with valid call sequences:
      //   [ visitCode ( visitFrame | visitXInsn | visitLabel | visitTryCatchBlock | visitLocalVariable | visitLineNumber )* visitMaxs ] visitEnd
      // In addition, the visitXInsn and visitLabel methods must be called in the sequential order of the bytecode instructions of the visited code,
      // visitTryCatchBlock must be called before the labels passed as arguments have been visited, and
      // the visitLocalVariable and visitLineNumber methods must be called after the labels passed as arguments have been visited.

      val hasAbstractBitSet = ((flags & asm.Opcodes.ACC_ABSTRACT) != 0)
      val hasCodeAttribute  = (!hasAbstractBitSet && !method.native)
      if (hasCodeAttribute) {

        jmethod.visitCode()

        if (emitVars && isClosureApply(method.symbol)) {
          // add a fake local for debugging purposes
          val outerField = clasz.symbol.info.decl(nme.OUTER_LOCAL)
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this =
              new Local(method.symbol.newVariable(nme.FAKE_LOCAL_THIS),
                        toTypeKind(outerField.tpe),
                        false)
            m.locals = m.locals ::: List(_this)
            computeLocalVarsIndex(m) // since we added a new local, we need to recompute indexes
            jmethod.visitVarInsn(asm.Opcodes.ALOAD, 0)
            jmethod.visitFieldInsn(asm.Opcodes.GETFIELD,
                                   javaName(clasz.symbol), // field owner
                                   javaName(outerField),   // field name
                                   descriptor(outerField)  // field descriptor
            )
            assert(_this.kind.isReferenceType, _this.kind)
            jmethod.visitVarInsn(asm.Opcodes.ASTORE, indexOf(_this))
          }
        }

        assert( m.locals forall { local => (m.params contains local) == local.arg }, m.locals )

        val hasStaticBitSet = ((flags & asm.Opcodes.ACC_STATIC) != 0)
        genCode(m, emitVars, hasStaticBitSet)

        // visitMaxs needs to be called according to the protocol. The arguments will be ignored
        // since maximums (and stack map frames) are computed. See ASM Guide, Section 3.2.1,
        // section "ClassWriter options"
        jmethod.visitMaxs(0, 0)
      }

      jmethod.visitEnd()

    }

    def addModuleInstanceField() {
      val fv =
        jclass.visitField(PublicStaticFinal, // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
                          strMODULE_INSTANCE_FIELD,
                          thisDescr,
                          null, // no java-generic-signature
                          null  // no initial value
        )

      // typestate: entering mode with valid call sequences:
      //   ( visitAnnotation | visitAttribute )* visitEnd.

      fv.visitEnd()
    }


    /* Typestate: should be called before being done with emitting fields (because it invokes addCreatorCode() which adds an IField to the current IClass). */
    def addStaticInit(mopt: Option[IMethod]) {

      val clinitMethod: asm.MethodVisitor = jclass.visitMethod(
        PublicStatic, // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
        CLASS_CONSTRUCTOR_NAME,
        mdesc_arglessvoid,
        null, // no java-generic-signature
        null  // no throwable exceptions
      )

      mopt match {

       	case Some(m) =>

          val oldLastBlock = m.lastBlock
          val lastBlock = m.newBlock()
          oldLastBlock.replaceInstruction(oldLastBlock.length - 1, JUMP(lastBlock))

          if (isStaticModule(clasz.symbol)) {
            // call object's private ctor from static ctor
            lastBlock emit NEW(REFERENCE(m.symbol.enclClass))
            lastBlock emit CALL_METHOD(m.symbol.enclClass.primaryConstructor, Static(onInstance = true))
          }

          if (isParcelableClass) { addCreatorCode(lastBlock) }

          lastBlock emit RETURN(UNIT)
          lastBlock.close()

          method = m
       	  jmethod = clinitMethod
          jMethodName = CLASS_CONSTRUCTOR_NAME
          jmethod.visitCode()
          computeLocalVarsIndex(m)
          genCode(m, emitVars = false, isStatic = true)
          jmethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
          jmethod.visitEnd()

       	case None =>
          clinitMethod.visitCode()
          legacyStaticInitializer(clinitMethod)
          clinitMethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
          clinitMethod.visitEnd()

      }
    }

    /* used only from addStaticInit() */
    private def legacyStaticInitializer(clinit: asm.MethodVisitor) {
      if (isStaticModule(clasz.symbol)) {
        clinit.visitTypeInsn(asm.Opcodes.NEW, thisName)
        clinit.visitMethodInsn(asm.Opcodes.INVOKESPECIAL,
                               thisName, INSTANCE_CONSTRUCTOR_NAME, mdesc_arglessvoid, false)
      }

      if (isParcelableClass) { legacyAddCreatorCode(clinit) }

      clinit.visitInsn(asm.Opcodes.RETURN)
    }

    // -----------------------------------------------------------------------------------------
    // Emitting bytecode instructions.
    // -----------------------------------------------------------------------------------------

    private def genConstant(mv: asm.MethodVisitor, const: Constant) {
      const.tag match {

        case BooleanTag => jcode.boolconst(const.booleanValue)

        case ByteTag    => jcode.iconst(const.byteValue.toInt)
        case ShortTag   => jcode.iconst(const.shortValue.toInt)
        case CharTag    => jcode.iconst(const.charValue)
        case IntTag     => jcode.iconst(const.intValue)

        case LongTag    => jcode.lconst(const.longValue)
        case FloatTag   => jcode.fconst(const.floatValue)
        case DoubleTag  => jcode.dconst(const.doubleValue)

        case UnitTag    => ()

        case StringTag  =>
          assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
          mv.visitLdcInsn(const.stringValue) // `stringValue` special-cases null, but not for a const with StringTag

        case NullTag    => mv.visitInsn(asm.Opcodes.ACONST_NULL)

        case ClazzTag   =>
          val kind = toTypeKind(const.typeValue)
          val toPush: asm.Type =
            if (kind.isValueType) classLiteral(kind)
            else javaType(kind)
          mv.visitLdcInsn(toPush)

        case EnumTag   =>
          val sym = const.symbolValue
          mv.visitFieldInsn(
            asm.Opcodes.GETSTATIC,
            javaName(sym.owner),
            javaName(sym),
            javaType(sym.tpe.underlying).getDescriptor()
          )

        case _ => abort("Unknown constant value: " + const)
      }
    }

    /** Just a namespace for utilities that encapsulate MethodVisitor idioms.
     *  In the ASM world, org.objectweb.asm.commons.InstructionAdapter plays a similar role,
     *  but the methods here allow choosing when to transition from ICode to ASM types
     *  (including not at all, e.g. for performance).
     */
    object jcode {

      import asm.Opcodes

      final def boolconst(b: Boolean) { iconst(if(b) 1 else 0) }

      def iconst(cst: Char) { iconst(cst.toInt) }
      def iconst(cst: Int) {
        if (cst >= -1 && cst <= 5) {
          jmethod.visitInsn(Opcodes.ICONST_0 + cst)
        } else if (cst >= java.lang.Byte.MIN_VALUE && cst <= java.lang.Byte.MAX_VALUE) {
          jmethod.visitIntInsn(Opcodes.BIPUSH, cst)
        } else if (cst >= java.lang.Short.MIN_VALUE && cst <= java.lang.Short.MAX_VALUE) {
          jmethod.visitIntInsn(Opcodes.SIPUSH, cst)
        } else {
          jmethod.visitLdcInsn(new Integer(cst))
        }
      }

      def lconst(cst: Long) {
        if (cst == 0L || cst == 1L) {
          jmethod.visitInsn(Opcodes.LCONST_0 + cst.asInstanceOf[Int])
        } else {
          jmethod.visitLdcInsn(new java.lang.Long(cst))
        }
      }

      def fconst(cst: Float) {
        val bits: Int = java.lang.Float.floatToIntBits(cst)
        if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
          jmethod.visitInsn(Opcodes.FCONST_0 + cst.asInstanceOf[Int])
        } else {
          jmethod.visitLdcInsn(new java.lang.Float(cst))
        }
      }

      def dconst(cst: Double) {
        val bits: Long = java.lang.Double.doubleToLongBits(cst)
        if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
          jmethod.visitInsn(Opcodes.DCONST_0 + cst.asInstanceOf[Int])
        } else {
          jmethod.visitLdcInsn(new java.lang.Double(cst))
        }
      }

      def newarray(elem: TypeKind) {
        if(elem.isRefOrArrayType) {
          jmethod.visitTypeInsn(Opcodes.ANEWARRAY, javaType(elem).getInternalName)
        } else {
          val rand = {
            if(elem.isIntSizedType) {
              (elem: @unchecked) match {
                case BOOL   => Opcodes.T_BOOLEAN
                case BYTE   => Opcodes.T_BYTE
                case SHORT  => Opcodes.T_SHORT
                case CHAR   => Opcodes.T_CHAR
                case INT    => Opcodes.T_INT
              }
            } else {
              (elem: @unchecked) match {
                case LONG   => Opcodes.T_LONG
                case FLOAT  => Opcodes.T_FLOAT
                case DOUBLE => Opcodes.T_DOUBLE
              }
            }
          }
          jmethod.visitIntInsn(Opcodes.NEWARRAY, rand)
        }
      }


      def load( idx: Int, tk: TypeKind) { emitVarInsn(Opcodes.ILOAD,  idx, tk) }
      def store(idx: Int, tk: TypeKind) { emitVarInsn(Opcodes.ISTORE, idx, tk) }

      def aload( tk: TypeKind) { emitTypeBased(aloadOpcodes,  tk) }
      def astore(tk: TypeKind) { emitTypeBased(astoreOpcodes, tk) }

      def neg(tk: TypeKind) { emitPrimitive(negOpcodes, tk) }
      def add(tk: TypeKind) { emitPrimitive(addOpcodes, tk) }
      def sub(tk: TypeKind) { emitPrimitive(subOpcodes, tk) }
      def mul(tk: TypeKind) { emitPrimitive(mulOpcodes, tk) }
      def div(tk: TypeKind) { emitPrimitive(divOpcodes, tk) }
      def rem(tk: TypeKind) { emitPrimitive(remOpcodes, tk) }

      def invokespecial(owner: String, name: String, desc: String) {
        jmethod.visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, desc, false)
      }
      def invokestatic(owner: String, name: String, desc: String) {
        jmethod.visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, desc, false)
      }
      def invokeinterface(owner: String, name: String, desc: String) {
        jmethod.visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, desc, true)
      }
      def invokevirtual(owner: String, name: String, desc: String) {
        jmethod.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, desc, false)
      }

      def goTo(label: asm.Label) { jmethod.visitJumpInsn(Opcodes.GOTO, label) }
      def emitIF(cond: TestOp, label: asm.Label)      { jmethod.visitJumpInsn(cond.opcodeIF(),     label) }
      def emitIF_ICMP(cond: TestOp, label: asm.Label) { jmethod.visitJumpInsn(cond.opcodeIFICMP(), label) }
      def emitIF_ACMP(cond: TestOp, label: asm.Label) {
        assert((cond == EQ) || (cond == NE), cond)
        val opc = (if(cond == EQ) Opcodes.IF_ACMPEQ else Opcodes.IF_ACMPNE)
        jmethod.visitJumpInsn(opc, label)
      }
      def emitIFNONNULL(label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNONNULL, label) }
      def emitIFNULL   (label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNULL,    label) }

      def emitRETURN(tk: TypeKind) {
        if(tk == UNIT) { jmethod.visitInsn(Opcodes.RETURN) }
        else           { emitTypeBased(returnOpcodes, tk)      }
      }

      /** Emits one of tableswitch or lookoupswitch. */
      def emitSWITCH(keys: Array[Int], branches: Array[asm.Label], defaultBranch: asm.Label, minDensity: Double) {
        assert(keys.length == branches.length)

        // For empty keys, it makes sense emitting LOOKUPSWITCH with defaultBranch only.
        // Similar to what javac emits for a switch statement consisting only of a default case.
        if (keys.length == 0) {
          jmethod.visitLookupSwitchInsn(defaultBranch, keys, branches)
          return
        }

        // sort `keys` by increasing key, keeping `branches` in sync. TODO FIXME use quicksort
        var i = 1
        while (i < keys.length) {
          var j = 1
          while (j <= keys.length - i) {
            if (keys(j) < keys(j - 1)) {
              val tmp     = keys(j)
              keys(j)     = keys(j - 1)
              keys(j - 1) = tmp
              val tmpL        = branches(j)
              branches(j)     = branches(j - 1)
              branches(j - 1) = tmpL
            }
            j += 1
          }
          i += 1
        }

        // check for duplicate keys to avoid "VerifyError: unsorted lookupswitch" (SI-6011)
        i = 1
        while (i < keys.length) {
          if(keys(i-1) == keys(i)) {
            abort("duplicate keys in SWITCH, can't pick arbitrarily one of them to evict, see SI-6011.")
          }
          i += 1
        }

        val keyMin = keys(0)
        val keyMax = keys(keys.length - 1)

        val isDenseEnough: Boolean = {
          /* Calculate in long to guard against overflow. TODO what overflow??? */
          val keyRangeD: Double = (keyMax.asInstanceOf[Long] - keyMin + 1).asInstanceOf[Double]
          val klenD:     Double = keys.length.toDouble
          val kdensity:  Double = (klenD / keyRangeD)

          kdensity >= minDensity
        }

        if (isDenseEnough) {
          // use a table in which holes are filled with defaultBranch.
          val keyRange    = (keyMax - keyMin + 1)
          val newBranches = new Array[asm.Label](keyRange)
          var oldPos = 0
          var i = 0
          while(i < keyRange) {
            val key = keyMin + i
            if (keys(oldPos) == key) {
              newBranches(i) = branches(oldPos)
              oldPos += 1
            } else {
              newBranches(i) = defaultBranch
            }
            i += 1
          }
          assert(oldPos == keys.length, "emitSWITCH")
          jmethod.visitTableSwitchInsn(keyMin, keyMax, defaultBranch, newBranches: _*)
        } else {
          jmethod.visitLookupSwitchInsn(defaultBranch, keys, branches)
        }
    }

      // internal helpers -- not part of the public API of `jcode`
      // don't make private otherwise inlining will suffer

      def emitVarInsn(opc: Int, idx: Int, tk: TypeKind) {
        assert((opc == Opcodes.ILOAD) || (opc == Opcodes.ISTORE), opc)
        jmethod.visitVarInsn(javaType(tk).getOpcode(opc), idx)
      }

      // ---------------- array load and store ----------------

      val aloadOpcodes  = { import Opcodes._; Array(AALOAD,  BALOAD,  SALOAD,  CALOAD,  IALOAD,  LALOAD,  FALOAD,  DALOAD)  }
      val astoreOpcodes = { import Opcodes._; Array(AASTORE, BASTORE, SASTORE, CASTORE, IASTORE, LASTORE, FASTORE, DASTORE) }

      val returnOpcodes = { import Opcodes._; Array(ARETURN, IRETURN, IRETURN, IRETURN, IRETURN, LRETURN, FRETURN, DRETURN) }

      def emitTypeBased(opcs: Array[Int], tk: TypeKind) {
        assert(tk != UNIT, tk)
        val opc = {
          if(tk.isRefOrArrayType) {   opcs(0) }
          else if(tk.isIntSizedType) {
            (tk: @unchecked) match {
              case BOOL | BYTE     => opcs(1)
              case SHORT           => opcs(2)
              case CHAR            => opcs(3)
              case INT             => opcs(4)
            }
          } else {
            (tk: @unchecked) match {
              case LONG            => opcs(5)
              case FLOAT           => opcs(6)
              case DOUBLE          => opcs(7)
            }
          }
        }
        jmethod.visitInsn(opc)
      }

      // ---------------- primitive operations ----------------

      val negOpcodes: Array[Int] = { import Opcodes._; Array(INEG, LNEG, FNEG, DNEG) }
      val addOpcodes: Array[Int] = { import Opcodes._; Array(IADD, LADD, FADD, DADD) }
      val subOpcodes: Array[Int] = { import Opcodes._; Array(ISUB, LSUB, FSUB, DSUB) }
      val mulOpcodes: Array[Int] = { import Opcodes._; Array(IMUL, LMUL, FMUL, DMUL) }
      val divOpcodes: Array[Int] = { import Opcodes._; Array(IDIV, LDIV, FDIV, DDIV) }
      val remOpcodes: Array[Int] = { import Opcodes._; Array(IREM, LREM, FREM, DREM) }

      def emitPrimitive(opcs: Array[Int], tk: TypeKind) {
        val opc = {
          if(tk.isIntSizedType) { opcs(0) }
          else {
            (tk: @unchecked) match {
              case LONG   => opcs(1)
              case FLOAT  => opcs(2)
              case DOUBLE => opcs(3)
            }
          }
        }
        jmethod.visitInsn(opc)
      }

    }

    /** Invoked from genMethod() and addStaticInit() */
    def genCode(m: IMethod,
                emitVars: Boolean, // this param name hides the instance-level var
                isStatic: Boolean) {


      newNormal.normalize(m)

      // ------------------------------------------------------------------------------------------------------------
      // Part 1 of genCode(): setting up one-to-one correspondence between ASM Labels and BasicBlocks `linearization`
      // ------------------------------------------------------------------------------------------------------------

      val linearization: List[BasicBlock] = linearizer.linearize(m)
      if(linearization.isEmpty) { return }

      var isModuleInitialized = false

      val labels: scala.collection.Map[BasicBlock, asm.Label] = mutable.HashMap(linearization map (_ -> new asm.Label()) : _*)

      val onePastLast = new asm.Label // token for the mythical instruction past the last instruction in the method being emitted

      // maps a BasicBlock b to the Label that corresponds to b's successor in the linearization. The last BasicBlock is mapped to the onePastLast label.
      val linNext: scala.collection.Map[BasicBlock, asm.Label] = {
        val result = mutable.HashMap.empty[BasicBlock, asm.Label]
        var rest = linearization
        var prev = rest.head
        rest = rest.tail
        while(!rest.isEmpty) {
          result += (prev -> labels(rest.head))
          prev = rest.head
          rest = rest.tail
        }
        assert(!result.contains(prev))
        result += (prev -> onePastLast)

        result
      }

      // ------------------------------------------------------------------------------------------------------------
      // Part 2 of genCode(): demarcating exception handler boundaries (visitTryCatchBlock() must be invoked before visitLabel() in genBlock())
      // ------------------------------------------------------------------------------------------------------------

        /* Generate exception handlers for the current method.
         *
         * Quoting from the JVMS 4.7.3 The Code Attribute
         * The items of the Code_attribute structure are as follows:
         *   . . .
         *   exception_table[]
         *     Each entry in the exception_table array describes one
         *     exception handler in the code array. The order of the handlers in
         *     the exception_table array is significant.
         *     Each exception_table entry contains the following four items:
         *       start_pc, end_pc:
         *         ... The value of end_pc either must be a valid index into
         *         the code array of the opcode of an instruction or must be equal to code_length,
         *         the length of the code array.
         *       handler_pc:
         *         The value of the handler_pc item indicates the start of the exception handler
         *       catch_type:
         *         ... If the value of the catch_type item is zero,
         *         this exception handler is called for all exceptions.
         *         This is used to implement finally
         */
        def genExceptionHandlers() {

          /* Return a list of pairs of intervals where the handler is active.
           * Each interval is closed on both ends, ie. inclusive both in the left and right endpoints: [start, end].
           * Preconditions:
           *   - e.covered non-empty
           * Postconditions for the result:
           *   - always non-empty
           *   - intervals are sorted as per `linearization`
           *   - the argument's `covered` blocks have been grouped into maximally contiguous intervals,
           *     ie. between any two intervals in the result there is a non-empty gap.
           *   - each of the `covered` blocks in the argument is contained in some interval in the result
           */
          def intervals(e: ExceptionHandler): List[BlockInteval] = {
            assert(e.covered.nonEmpty, e)
            var result: List[BlockInteval] = Nil
            var rest = linearization

            // find intervals
            while(!rest.isEmpty) {
              // find interval start
              var start: BasicBlock = null
              while(!rest.isEmpty && (start eq null)) {
                if(e.covered(rest.head)) { start = rest.head }
                rest = rest.tail
              }
              if(start ne null) {
                // find interval end
                var end = start // for the time being
                while(!rest.isEmpty && (e.covered(rest.head))) {
                  end  = rest.head
                  rest = rest.tail
                }
                result = BlockInteval(start, end) :: result
              }
            }

            assert(result.nonEmpty, e)

            result
          }

          /* TODO test/files/run/exceptions-2.scala displays an ExceptionHandler.covered that contains
           * blocks not in the linearization (dead-code?). Is that well-formed or not?
           * For now, we ignore those blocks (after all, that's what `genBlocks(linearization)` in effect does).
           */
          for (e <- this.method.exh) {
            val ignore: Set[BasicBlock] = (e.covered filterNot { b => linearization contains b } )
            // TODO someday assert(ignore.isEmpty, "an ExceptionHandler.covered contains blocks not in the linearization (dead-code?)")
            if(ignore.nonEmpty) {
              e.covered  = e.covered filterNot ignore
            }
          }

          // an ExceptionHandler lacking covered blocks doesn't get an entry in the Exceptions table.
          // TODO in that case, ExceptionHandler.cls doesn't go through javaName(). What if cls is an inner class?
          for (e <- this.method.exh ; if e.covered.nonEmpty ; p <- intervals(e)) {
            debuglog("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
                     " from: " + p.start + " to: " + p.end + " catching: " + e.cls)
            val cls: String = if (e.cls == NoSymbol || e.cls == ThrowableClass) null
                              else javaName(e.cls)
            jmethod.visitTryCatchBlock(labels(p.start), linNext(p.end), labels(e.startBlock), cls)
          }
        } // end of genCode()'s genExceptionHandlers()

      if (m.exh.nonEmpty) { genExceptionHandlers() }

      // ------------------------------------------------------------------------------------------------------------
      // Part 3 of genCode(): "Infrastructure" to later emit debug info for local variables and method params (LocalVariablesTable bytecode attribute).
      // ------------------------------------------------------------------------------------------------------------

        case class LocVarEntry(local: Local, start: asm.Label, end: asm.Label) // start is inclusive while end exclusive.

        case class Interval(lstart: asm.Label, lend: asm.Label) {
          final def start = lstart.getOffset
          final def end   = lend.getOffset

          def precedes(that: Interval): Boolean = { this.end < that.start }

          def overlaps(that: Interval): Boolean = { !(this.precedes(that) || that.precedes(this)) }

          def mergeWith(that: Interval): Interval = {
            val newStart = if(this.start <= that.start) this.lstart else that.lstart
            val newEnd   = if(this.end   <= that.end)   that.lend   else this.lend
            Interval(newStart, newEnd)
          }

          def repOK: Boolean = { start <= end }

        }

        /** Track those instruction ranges where certain locals are in scope. Used to later emit the LocalVariableTable attribute (JVMS 4.7.13) */
        object scoping {

          private val pending = mutable.Map.empty[Local, mutable.Stack[Label]]
          private var seen: List[LocVarEntry] = Nil

          private def fuse(ranges: List[Interval], added: Interval): List[Interval] = {
            assert(added.repOK, added)
            if(ranges.isEmpty) { return List(added) }
            // precond: ranges is sorted by increasing start
            var fused: List[Interval] = Nil
            var done = false
            var rest = ranges
            while(!done && rest.nonEmpty) {
              val current = rest.head
              assert(current.repOK, current)
              rest = rest.tail
              if(added precedes current) {
                fused = fused ::: ( added :: current :: rest )
                done = true
              } else if(current overlaps added) {
                fused = fused ::: ( added.mergeWith(current) :: rest )
                done = true
              }
            }
            if(!done) { fused = fused ::: List(added) }
            assert(repOK(fused), fused)

            fused
          }

          def pushScope(lv: Local, start: Label) {
            val st = pending.getOrElseUpdate(lv, mutable.Stack.empty[Label])
            st.push(start)
          }
          def popScope(lv: Local, end: Label, iPos: Position) {
            pending.get(lv) match {
              case Some(st) if st.nonEmpty =>
                val start = st.pop()
                seen ::= LocVarEntry(lv, start, end)
              case _ =>
                // TODO SI-6049 track down the cause for these.
                devWarning(s"$iPos: Visited SCOPE_EXIT before visiting corresponding SCOPE_ENTER. SI-6191")
            }
          }

          def getMerged(): scala.collection.Map[Local, List[Interval]] = {
            // TODO should but isn't: unbalanced start(s) of scope(s)
            val shouldBeEmpty = pending filter { p => val (_, st) = p; st.nonEmpty }
            val merged = mutable.Map[Local, List[Interval]]()
            def addToMerged(lv: Local, start: Label, end: Label) {
              val intv   = Interval(start, end)
              merged(lv) = if (merged contains lv) fuse(merged(lv), intv) else intv :: Nil
            }
            for(LocVarEntry(lv, start, end) <- seen) { addToMerged(lv, start, end) }

            /* for each var with unbalanced start(s) of scope(s):
                 (a) take the earliest start (among unbalanced and balanced starts)
                 (b) take the latest end (onePastLast if none available)
                 (c) merge the thus made-up interval
             */
            for((k, st) <- shouldBeEmpty) {
              var start = st.toList.sortBy(_.getOffset).head
              if(merged.isDefinedAt(k)) {
                val balancedStart = merged(k).head.lstart
                if(balancedStart.getOffset < start.getOffset) {
                  start = balancedStart
                }
              }
              val endOpt: Option[Label] = for(ranges <- merged.get(k)) yield ranges.last.lend
              val end = endOpt.getOrElse(onePastLast)
              addToMerged(k, start, end)
            }

            merged
          }

          private def repOK(fused: List[Interval]): Boolean = {
            fused match {
              case Nil      => true
              case h :: Nil => h.repOK
              case h :: n :: rest =>
                h.repOK && h.precedes(n) && !h.overlaps(n) && repOK(n :: rest)
            }
          }

        }

      def genLocalVariableTable() {
        // adding `this` and method params.
        if (!isStatic) {
          jmethod.visitLocalVariable("this", thisDescr, null, labels(m.startBlock), onePastLast, 0)
        }
        for(lv <- m.params) {
          jmethod.visitLocalVariable(javaName(lv.sym), descriptor(lv.kind), null, labels(m.startBlock), onePastLast, indexOf(lv))
        }
        // adding non-param locals
        var anonCounter = 0
        var fltnd: List[Tuple3[String, Local, Interval]] = Nil
        for((local, ranges) <- scoping.getMerged()) {
          var name = javaName(local.sym)
          if (name == null) {
            anonCounter += 1
            name = "<anon" + anonCounter + ">"
          }
          for(intrvl <- ranges) {
            fltnd ::= (name, local, intrvl)
          }
        }
        // quest for deterministic output that Map.toList doesn't provide (so that ant test.stability doesn't complain).
        val srtd = fltnd.sortBy { kr =>
          val (name: String, _, intrvl: Interval) = kr

          (intrvl.start, intrvl.end - intrvl.start, name)  // ie sort by (start, length, name)
        }

        for((name, local, Interval(start, end)) <- srtd) {
          jmethod.visitLocalVariable(name, descriptor(local.kind), null, start, end, indexOf(local))
        }
        // "There may be no more than one LocalVariableTable attribute per local variable in the Code attribute"
      }

      // ------------------------------------------------------------------------------------------------------------
      // Part 4 of genCode(): Bookkeeping (to later emit debug info) of association between line-number and instruction position.
      // ------------------------------------------------------------------------------------------------------------

      case class LineNumberEntry(line: Int, start: asm.Label)
      var lastLineNr: Int = -1
      var lnEntries: List[LineNumberEntry] = Nil

      // ------------------------------------------------------------------------------------------------------------
      // Part 5 of genCode(): "Utilities" to emit code proper (most prominently: genBlock()).
      // ------------------------------------------------------------------------------------------------------------

      var nextBlock: BasicBlock = linearization.head

      def genBlocks(l: List[BasicBlock]): Unit = l match {
        case Nil => ()
        case x :: Nil => nextBlock = null; genBlock(x)
        case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys)
      }

      def genCallMethod(call: CALL_METHOD) {
        val CALL_METHOD(method, style) = call
        val siteSymbol  = clasz.symbol
        val hostSymbol  = call.hostClass
        val methodOwner = method.owner
        // info calls so that types are up to date; erasure may add lateINTERFACE to traits
        hostSymbol.info ; methodOwner.info

        def needsInterfaceCall(sym: Symbol) = (
             sym.isInterface
          || sym.isJavaDefined && sym.isNonBottomSubClass(ClassfileAnnotationClass)
        )
        // whether to reference the type of the receiver or
        // the type of the method owner
        val useMethodOwner = (
             style != Dynamic
          || hostSymbol.isBottomClass
          || methodOwner == ObjectClass
        )
        val receiver = if (useMethodOwner) methodOwner else hostSymbol
        val jowner   = javaName(receiver)
        val jname    = javaName(method)
        val jtype    = javaType(method).getDescriptor()

        def dbg(invoke: String) {
          debuglog("%s %s %s.%s:%s".format(invoke, receiver.accessString, jowner, jname, jtype))
        }

        def initModule() {
          // we initialize the MODULE$ field immediately after the super ctor
          if (isStaticModule(siteSymbol) && !isModuleInitialized &&
              jMethodName == INSTANCE_CONSTRUCTOR_NAME &&
              jname == INSTANCE_CONSTRUCTOR_NAME) {
            isModuleInitialized = true
            jmethod.visitVarInsn(asm.Opcodes.ALOAD, 0)
            jmethod.visitFieldInsn(asm.Opcodes.PUTSTATIC, thisName, strMODULE_INSTANCE_FIELD, thisDescr)
          }
        }

        style match {
          case Static(true)                            => dbg("invokespecial");  jcode.invokespecial  (jowner, jname, jtype)
          case Static(false)                           => dbg("invokestatic");   jcode.invokestatic   (jowner, jname, jtype)
          case Dynamic if needsInterfaceCall(receiver) => dbg("invokinterface"); jcode.invokeinterface(jowner, jname, jtype)
          case Dynamic                                 => dbg("invokevirtual");  jcode.invokevirtual  (jowner, jname, jtype)
          case SuperCall(_)                            =>
            dbg("invokespecial")
            jcode.invokespecial(jowner, jname, jtype)
            initModule()
        }
      } // end of genCode()'s genCallMethod()

      def genBlock(b: BasicBlock) {
        jmethod.visitLabel(labels(b))

        debuglog("Generating code for block: " + b)

        // val lastInstr = b.lastInstruction

        for (instr <- b) {

          if(instr.pos.isDefined) {
            val iPos = instr.pos
            val currentLineNr = iPos.line
            val skip = (currentLineNr == lastLineNr) // if(iPos.isRange) iPos.sameRange(lastPos) else
            if(!skip) {
              lastLineNr = currentLineNr
              val lineLab = new asm.Label
              jmethod.visitLabel(lineLab)
              lnEntries ::= LineNumberEntry(iPos.finalPosition.line, lineLab)
            }
          }

          genInstr(instr, b)

        }

      }

      def genInstr(instr: Instruction, b: BasicBlock) {
        import asm.Opcodes
        (instr.category: @scala.annotation.switch) match {


          case icodes.localsCat =>
          def genLocalInstr() = (instr: @unchecked) match {
            case THIS(_) => jmethod.visitVarInsn(Opcodes.ALOAD, 0)
            case LOAD_LOCAL(local) => jcode.load(indexOf(local), local.kind)
            case STORE_LOCAL(local) => jcode.store(indexOf(local), local.kind)
            case STORE_THIS(_) =>
              // this only works for impl classes because the self parameter comes first
              // in the method signature. If that changes, this code has to be revisited.
              jmethod.visitVarInsn(Opcodes.ASTORE, 0)

            case SCOPE_ENTER(lv) =>
              // locals removed by closelim (via CopyPropagation) may have left behind SCOPE_ENTER, SCOPE_EXIT that are to be ignored
              val relevant = (!lv.sym.isSynthetic && m.locals.contains(lv))
              if (relevant) { // TODO check: does GenICode emit SCOPE_ENTER, SCOPE_EXIT for synthetic vars?
                // this label will have DEBUG bit set in its flags (ie ASM ignores it for dataflow purposes)
                // similarly, these labels aren't tracked in the `labels` map.
                val start = new asm.Label
                jmethod.visitLabel(start)
                scoping.pushScope(lv, start)
              }

            case SCOPE_EXIT(lv) =>
              val relevant = (!lv.sym.isSynthetic && m.locals.contains(lv))
              if (relevant) {
                // this label will have DEBUG bit set in its flags (ie ASM ignores it for dataflow purposes)
                // similarly, these labels aren't tracked in the `labels` map.
                val end = new asm.Label
                jmethod.visitLabel(end)
                scoping.popScope(lv, end, instr.pos)
              }
          }
          genLocalInstr()

          case icodes.stackCat =>
          def genStackInstr() = (instr: @unchecked) match {

            case LOAD_MODULE(module) =>
              // assert(module.isModule, "Expected module: " + module)
              debuglog("generating LOAD_MODULE for: " + module + " flags: " + module.flagString)
              def inStaticMethod = this.method != null && this.method.symbol.isStaticMember
              if (clasz.symbol == module.moduleClass && jMethodName != nme.readResolve.toString && !inStaticMethod) {
                jmethod.visitVarInsn(Opcodes.ALOAD, 0)
              } else {
                jmethod.visitFieldInsn(
                  Opcodes.GETSTATIC,
                  javaName(module) /* + "$" */ ,
                  strMODULE_INSTANCE_FIELD,
                  descriptor(module))
              }

            case DROP(kind) => emit(if (kind.isWideType) Opcodes.POP2 else Opcodes.POP)

            case DUP(kind) => emit(if (kind.isWideType) Opcodes.DUP2 else Opcodes.DUP)

            case LOAD_EXCEPTION(_) => ()
          }
          genStackInstr()

          case icodes.constCat => genConstant(jmethod, instr.asInstanceOf[CONSTANT].constant)

          case icodes.arilogCat => genPrimitive(instr.asInstanceOf[CALL_PRIMITIVE].primitive, instr.pos)

          case icodes.castsCat =>
          def genCastInstr() = (instr: @unchecked) match {

            case IS_INSTANCE(tpe) =>
              val jtyp: asm.Type =
                tpe match {
                  case REFERENCE(cls) => asm.Type.getObjectType(javaName(cls))
                  case ARRAY(elem) => javaArrayType(javaType(elem))
                  case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
                }
              jmethod.visitTypeInsn(Opcodes.INSTANCEOF, jtyp.getInternalName)

            case CHECK_CAST(tpe) =>
              tpe match {

                case REFERENCE(cls) =>
                  if (cls != ObjectClass) { // No need to checkcast for Objects
                    jmethod.visitTypeInsn(Opcodes.CHECKCAST, javaName(cls))
                  }

                case ARRAY(elem) =>
                  val iname = javaArrayType(javaType(elem)).getInternalName
                  jmethod.visitTypeInsn(Opcodes.CHECKCAST, iname)

                case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe)
              }

          }
          genCastInstr()

          case icodes.objsCat =>
          def genObjsInstr() = (instr: @unchecked) match {
            case BOX(kind) =>
              val MethodNameAndType(mname, mdesc) = jBoxTo(kind)
              jcode.invokestatic(BoxesRunTime, mname, mdesc)

            case UNBOX(kind) =>
              val MethodNameAndType(mname, mdesc) = jUnboxTo(kind)
              jcode.invokestatic(BoxesRunTime, mname, mdesc)

            case NEW(REFERENCE(cls)) =>
              val className = javaName(cls)
              jmethod.visitTypeInsn(Opcodes.NEW, className)

            case MONITOR_ENTER() => emit(Opcodes.MONITORENTER)
            case MONITOR_EXIT() => emit(Opcodes.MONITOREXIT)
          }
          genObjsInstr()

          case icodes.fldsCat =>
          def genFldsInstr() = (instr: @unchecked) match {

            case lf @ LOAD_FIELD(field, isStatic) =>
              val owner = javaName(lf.hostClass)
              debuglog("LOAD_FIELD with owner: " + owner + " flags: " + field.owner.flagString)
              val fieldJName = javaName(field)
              val fieldDescr = descriptor(field)
              val opc = if (isStatic) Opcodes.GETSTATIC else Opcodes.GETFIELD
              jmethod.visitFieldInsn(opc, owner, fieldJName, fieldDescr)

            case STORE_FIELD(field, isStatic) =>
              val owner = javaName(field.owner)
              val fieldJName = javaName(field)
              val fieldDescr = descriptor(field)
              val opc = if (isStatic) Opcodes.PUTSTATIC else Opcodes.PUTFIELD
              jmethod.visitFieldInsn(opc, owner, fieldJName, fieldDescr)

          }
          genFldsInstr()

          case icodes.mthdsCat =>
          def genMethodsInstr() = (instr: @unchecked) match {

            /* Special handling to access native Array.clone() */
            case call @ CALL_METHOD(definitions.Array_clone, Dynamic) =>
              val target: String = javaType(call.targetTypeKind).getInternalName
              jcode.invokevirtual(target, "clone", mdesc_arrayClone)

            case call @ CALL_METHOD(method, style) => genCallMethod(call)

          }
          genMethodsInstr()

          case icodes.arraysCat =>
          def genArraysInstr() = (instr: @unchecked) match {
            case LOAD_ARRAY_ITEM(kind) => jcode.aload(kind)
            case STORE_ARRAY_ITEM(kind) => jcode.astore(kind)
            case CREATE_ARRAY(elem, 1) => jcode newarray elem
            case CREATE_ARRAY(elem, dims) => jmethod.visitMultiANewArrayInsn(descriptor(ArrayN(elem, dims)), dims)
          }
          genArraysInstr()

          case icodes.jumpsCat =>
          def genJumpInstr() = (instr: @unchecked) match {

            case sw @ SWITCH(tagss, branches) =>
              assert(branches.length == tagss.length + 1, sw)
              val flatSize = sw.flatTagsCount
              val flatKeys = new Array[Int](flatSize)
              val flatBranches = new Array[asm.Label](flatSize)

              var restTagss = tagss
              var restBranches = branches
              var k = 0 // ranges over flatKeys and flatBranches
              while (restTagss.nonEmpty) {
                val currLabel = labels(restBranches.head)
                for (cTag <- restTagss.head) {
                  flatKeys(k) = cTag
                  flatBranches(k) = currLabel
                  k += 1
                }
                restTagss = restTagss.tail
                restBranches = restBranches.tail
              }
              val defaultLabel = labels(restBranches.head)
              assert(restBranches.tail.isEmpty)
              debuglog("Emitting SWITCH:\ntags: " + tagss + "\nbranches: " + branches)
              jcode.emitSWITCH(flatKeys, flatBranches, defaultLabel, MIN_SWITCH_DENSITY)

            case JUMP(whereto) =>
              if (nextBlock != whereto)
                jcode goTo labels(whereto)
                // SI-6102: Determine whether eliding this JUMP results in an empty range being covered by some EH.
                // If so, emit a NOP in place of the elided JUMP, to avoid "java.lang.ClassFormatError: Illegal exception table range"
              else if (newNormal.isJumpOnly(b) && m.exh.exists(eh => eh.covers(b))) {
                devWarning("Had a jump only block that wasn't collapsed")
                emit(asm.Opcodes.NOP)
              }

            case CJUMP(success, failure, cond, kind) =>
              if (kind.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(cond.negate(), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(cond, labels(success))
                  if (nextBlock != failure) { jcode goTo labels(failure) }
                }
              } else if (kind.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(cond.negate(), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(cond, labels(success))
                  if (nextBlock != failure) { jcode goTo labels(failure) }
                }
              } else {
                (kind: @unchecked) match {
                  case LONG => emit(Opcodes.LCMP)
                  case FLOAT =>
                    if (cond == LT || cond == LE) emit(Opcodes.FCMPG)
                    else emit(Opcodes.FCMPL)
                  case DOUBLE =>
                    if (cond == LT || cond == LE) emit(Opcodes.DCMPG)
                    else emit(Opcodes.DCMPL)
                }
                if (nextBlock == success) {
                  jcode.emitIF(cond.negate(), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(cond, labels(success))
                  if (nextBlock != failure) { jcode goTo labels(failure) }
                }
              }

            case CZJUMP(success, failure, cond, kind) =>
              if (kind.isIntSizedType) { // BOOL, BYTE, CHAR, SHORT, or INT
                if (nextBlock == success) {
                  jcode.emitIF(cond.negate(), labels(failure))
                } else {
                  jcode.emitIF(cond, labels(success))
                  if (nextBlock != failure) { jcode goTo labels(failure) }
                }
              } else if (kind.isRefOrArrayType) { // REFERENCE(_) | ARRAY(_)
                val Success = success
                val Failure = failure
                // @unchecked because references aren't compared with GT, GE, LT, LE.
                ((cond, nextBlock): @unchecked) match {
                  case (EQ, Success) => jcode emitIFNONNULL labels(failure)
                  case (NE, Failure) => jcode emitIFNONNULL labels(success)
                  case (EQ, Failure) => jcode emitIFNULL labels(success)
                  case (NE, Success) => jcode emitIFNULL labels(failure)
                  case (EQ, _) =>
                    jcode emitIFNULL labels(success)
                    jcode goTo labels(failure)
                  case (NE, _) =>
                    jcode emitIFNONNULL labels(success)
                    jcode goTo labels(failure)
                }
              } else {
                (kind: @unchecked) match {
                  case LONG =>
                    emit(Opcodes.LCONST_0)
                    emit(Opcodes.LCMP)
                  case FLOAT =>
                    emit(Opcodes.FCONST_0)
                    if (cond == LT || cond == LE) emit(Opcodes.FCMPG)
                    else emit(Opcodes.FCMPL)
                  case DOUBLE =>
                    emit(Opcodes.DCONST_0)
                    if (cond == LT || cond == LE) emit(Opcodes.DCMPG)
                    else emit(Opcodes.DCMPL)
                }
                if (nextBlock == success) {
                  jcode.emitIF(cond.negate(), labels(failure))
                } else {
                  jcode.emitIF(cond, labels(success))
                  if (nextBlock != failure) { jcode goTo labels(failure) }
                }
              }

          }
          genJumpInstr()

          case icodes.retCat =>
          def genRetInstr() = (instr: @unchecked) match {
            case RETURN(kind) => jcode emitRETURN kind
            case THROW(_) => emit(Opcodes.ATHROW)
          }
          genRetInstr()
        }
      }

      /*
       * Emits one or more conversion instructions based on the types given as arguments.
       *
       * @param from The type of the value to be converted into another type.
       * @param to   The type the value will be converted into.
       */
      def emitT2T(from: TypeKind, to: TypeKind) {
        assert(isNonUnitValueTK(from) && isNonUnitValueTK(to), s"Cannot emit primitive conversion from $from to $to")

            def pickOne(opcs: Array[Int]) {
              val chosen = (to: @unchecked) match {
                case BYTE   => opcs(0)
                case SHORT  => opcs(1)
                case CHAR   => opcs(2)
                case INT    => opcs(3)
                case LONG   => opcs(4)
                case FLOAT  => opcs(5)
                case DOUBLE => opcs(6)
              }
              if(chosen != -1) { emit(chosen) }
            }

        if(from == to) { return }
        // the only conversion involving BOOL that is allowed is (BOOL -> BOOL)
        assert(from != BOOL && to != BOOL, s"inconvertible types : $from -> $to")

        if(from.isIntSizedType) { // BYTE, CHAR, SHORT, and INT. (we're done with BOOL already)

          val fromByte  = { import asm.Opcodes._; Array( -1,  -1, I2C,  -1, I2L, I2F, I2D) } // do nothing for (BYTE -> SHORT) and for (BYTE -> INT)
          val fromChar  = { import asm.Opcodes._; Array(I2B, I2S,  -1,  -1, I2L, I2F, I2D) } // for (CHAR  -> INT) do nothing
          val fromShort = { import asm.Opcodes._; Array(I2B,  -1, I2C,  -1, I2L, I2F, I2D) } // for (SHORT -> INT) do nothing
          val fromInt   = { import asm.Opcodes._; Array(I2B, I2S, I2C,  -1, I2L, I2F, I2D) }

          (from: @unchecked) match {
            case BYTE  => pickOne(fromByte)
            case SHORT => pickOne(fromShort)
            case CHAR  => pickOne(fromChar)
            case INT   => pickOne(fromInt)
          }

        } else { // FLOAT, LONG, DOUBLE

          (from: @unchecked) match {
            case FLOAT           =>
              import asm.Opcodes.{ F2L, F2D, F2I }
              (to: @unchecked) match {
                case LONG    => emit(F2L)
                case DOUBLE  => emit(F2D)
                case _       => emit(F2I); emitT2T(INT, to)
              }

            case LONG            =>
              import asm.Opcodes.{ L2F, L2D, L2I }
              (to: @unchecked) match {
                case FLOAT   => emit(L2F)
                case DOUBLE  => emit(L2D)
                case _       => emit(L2I); emitT2T(INT, to)
              }

            case DOUBLE          =>
              import asm.Opcodes.{ D2L, D2F, D2I }
              (to: @unchecked) match {
                case FLOAT   => emit(D2F)
                case LONG    => emit(D2L)
                case _       => emit(D2I); emitT2T(INT, to)
              }
          }
        }
      } // end of genCode()'s emitT2T()

      def genPrimitive(primitive: Primitive, pos: Position) {

        import asm.Opcodes

        primitive match {

          case Negation(kind) => jcode.neg(kind)

          case Arithmetic(op, kind) =>
            def genArith() = {
            op match {

              case ADD => jcode.add(kind)
              case SUB => jcode.sub(kind)
              case MUL => jcode.mul(kind)
              case DIV => jcode.div(kind)
              case REM => jcode.rem(kind)

              case NOT =>
                if(kind.isIntSizedType) {
                  emit(Opcodes.ICONST_M1)
                  emit(Opcodes.IXOR)
                } else if(kind == LONG) {
                  jmethod.visitLdcInsn(new java.lang.Long(-1))
                  jmethod.visitInsn(Opcodes.LXOR)
                } else {
                  abort("Impossible to negate an " + kind)
                }

              case _ =>
                abort("Unknown arithmetic primitive " + primitive)
            }
            }
            genArith()

          // TODO Logical's 2nd elem should be declared ValueTypeKind, to better approximate its allowed values (isIntSized, its comments appears to convey)
          // TODO GenICode uses `toTypeKind` to define that elem, `toValueTypeKind` would be needed instead.
          // TODO How about adding some asserts to Logical and similar ones to capture the remaining constraint (UNIT not allowed).
          case Logical(op, kind) =>
            def genLogical() = op match {
              case AND =>
                kind match {
                  case LONG => emit(Opcodes.LAND)
                  case INT  => emit(Opcodes.IAND)
                  case _    =>
                    emit(Opcodes.IAND)
                    if (kind != BOOL) { emitT2T(INT, kind) }
                }
              case OR =>
                kind match {
                  case LONG => emit(Opcodes.LOR)
                  case INT  => emit(Opcodes.IOR)
                  case _ =>
                    emit(Opcodes.IOR)
                    if (kind != BOOL) { emitT2T(INT, kind) }
                }
              case XOR =>
                kind match {
                  case LONG => emit(Opcodes.LXOR)
                  case INT  => emit(Opcodes.IXOR)
                  case _ =>
                    emit(Opcodes.IXOR)
                    if (kind != BOOL) { emitT2T(INT, kind) }
                }
            }
            genLogical()

          case Shift(op, kind) =>
            def genShift() = op match {
              case LSL =>
                kind match {
                  case LONG => emit(Opcodes.LSHL)
                  case INT  => emit(Opcodes.ISHL)
                  case _ =>
                    emit(Opcodes.ISHL)
                    emitT2T(INT, kind)
                }
              case ASR =>
                kind match {
                  case LONG => emit(Opcodes.LSHR)
                  case INT  => emit(Opcodes.ISHR)
                  case _ =>
                    emit(Opcodes.ISHR)
                    emitT2T(INT, kind)
                }
              case LSR =>
                kind match {
                  case LONG => emit(Opcodes.LUSHR)
                  case INT  => emit(Opcodes.IUSHR)
                  case  _ =>
                    emit(Opcodes.IUSHR)
                    emitT2T(INT, kind)
                }
            }
            genShift()

          case Comparison(op, kind) =>
            def genCompare() = op match {
              case CMP =>
                (kind: @unchecked) match {
                  case LONG =>  emit(Opcodes.LCMP)
                }
              case CMPL =>
                (kind: @unchecked) match {
                  case FLOAT  => emit(Opcodes.FCMPL)
                  case DOUBLE => emit(Opcodes.DCMPL)
                }
              case CMPG =>
                (kind: @unchecked) match {
                  case FLOAT  => emit(Opcodes.FCMPG)
                  case DOUBLE => emit(Opcodes.DCMPL) // TODO bug? why not DCMPG? http://docs.oracle.com/javase/specs/jvms/se6/html/Instructions2.doc3.html

                }
            }
            genCompare()

          case Conversion(src, dst) =>
            debuglog("Converting from: " + src + " to: " + dst)
            emitT2T(src, dst)

          case ArrayLength(_) => emit(Opcodes.ARRAYLENGTH)

          case StartConcat =>
            jmethod.visitTypeInsn(Opcodes.NEW, StringBuilderClassName)
            jmethod.visitInsn(Opcodes.DUP)
            jcode.invokespecial(
              StringBuilderClassName,
              INSTANCE_CONSTRUCTOR_NAME,
              mdesc_arglessvoid
            )

          case StringConcat(el) =>
            val jtype = el match {
              case REFERENCE(_) | ARRAY(_) => JAVA_LANG_OBJECT
              case _ => javaType(el)
            }
            jcode.invokevirtual(
              StringBuilderClassName,
              "append",
              asm.Type.getMethodDescriptor(StringBuilderType, Array(jtype): _*)
            )

          case EndConcat =>
            jcode.invokevirtual(StringBuilderClassName, "toString", mdesc_toString)

          case _ => abort("Unimplemented primitive " + primitive)
        }
      } // end of genCode()'s genPrimitive()

      // ------------------------------------------------------------------------------------------------------------
      // Part 6 of genCode(): the executable part of genCode() starts here.
      // ------------------------------------------------------------------------------------------------------------

      genBlocks(linearization)

      jmethod.visitLabel(onePastLast)

      if(emitLines) {
        for(LineNumberEntry(line, start) <- lnEntries.sortBy(_.start.getOffset)) { jmethod.visitLineNumber(line, start) }
      }
      if(emitVars)  { genLocalVariableTable() }

    } // end of BytecodeGenerator.genCode()


    ////////////////////// local vars ///////////////////////

    def sizeOf(k: TypeKind): Int = if(k.isWideType) 2 else 1

    final def indexOf(local: Local): Int = {
      assert(local.index >= 0, "Invalid index for: " + local + "{" + local.## + "}: ")
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given method.
     * *Does not assume the parameters come first!*
     */
    def computeLocalVarsIndex(m: IMethod) {
      var idx = if (m.symbol.isStaticMember) 0 else 1

      for (l <- m.params) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }

      for (l <- m.locals if !l.arg) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }
    }

  } // end of class JPlainBuilder


  /** builder of mirror classes */
  class JMirrorBuilder(bytecodeWriter: BytecodeWriter, needsOutfile: Boolean) extends JCommonBuilder(bytecodeWriter, needsOutfile) {

    private var cunit: CompilationUnit = _
    def getCurrentCUnit(): CompilationUnit = cunit

    /** Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     */
    def genMirrorClass(modsym: Symbol, cunit: CompilationUnit) {
      assert(modsym.companionClass == NoSymbol, modsym)
      innerClassBuffer.clear()
      this.cunit = cunit
      val moduleName = javaName(modsym) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)

      val flags = (asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL)
      val mirrorClass = createJClass(flags,
                                     mirrorName,
                                     null /* no java-generic-signature */,
                                     JAVA_LANG_OBJECT.getInternalName,
                                     EMPTY_STRING_ARRAY)

      log(s"Dumping mirror class for '$mirrorName'")

      // typestate: entering mode with valid call sequences:
      //   [ visitSource ] [ visitOuterClass ] ( visitAnnotation | visitAttribute )*

      if(emitSource) {
        mirrorClass.visitSource("" + cunit.source,
                                null /* SourceDebugExtension */)
      }

      val ssa = getAnnotPickle(mirrorName, modsym.companionSymbol)
      mirrorClass.visitAttribute(if(ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, modsym.annotations ++ ssa)

      // typestate: entering mode with valid call sequences:
      //   ( visitInnerClass | visitField | visitMethod )* visitEnd

      addForwarders(isRemote(modsym), mirrorClass, mirrorName, modsym)

      addInnerClasses(modsym, mirrorClass, isMirror = true)
      mirrorClass.visitEnd()
      writeIfNotTooBig("" + modsym.name, mirrorName, mirrorClass, modsym)
    }
  } // end of class JMirrorBuilder


  /** builder of bean info classes */
  class JBeanInfoBuilder(bytecodeWriter: BytecodeWriter, needsOutfile: Boolean) extends JBuilder(bytecodeWriter, needsOutfile) {

    /**
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     */
    def genBeanInfoClass(clasz: IClass) {

      // val BeanInfoSkipAttr    = definitions.getRequiredClass("scala.beans.BeanInfoSkip")
      // val BeanDisplayNameAttr = definitions.getRequiredClass("scala.beans.BeanDisplayName")
      // val BeanDescriptionAttr = definitions.getRequiredClass("scala.beans.BeanDescription")
      // val description = c.symbol getAnnotation BeanDescriptionAttr
      // informProgress(description.toString)
      innerClassBuffer.clear()

      val flags = mkFlags(
        javaFlags(clasz.symbol),
        if(isDeprecated(clasz.symbol)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
      )

      val beanInfoName = (javaName(clasz.symbol) + "BeanInfo")
      val beanInfoClass = createJClass(
            flags,
            beanInfoName,
            null, // no java-generic-signature
            "scala/beans/ScalaBeanInfo",
            EMPTY_STRING_ARRAY
      )

      // beanInfoClass typestate: entering mode with valid call sequences:
      //   [ visitSource ] [ visitOuterClass ] ( visitAnnotation | visitAttribute )*

      beanInfoClass.visitSource(
        clasz.cunit.source.toString,
        null /* SourceDebugExtension */
      )

      var fieldList = List[String]()

      for (f <- clasz.fields if f.symbol.hasGetter;
                 g = f.symbol.getterIn(clasz.symbol);
                 s = f.symbol.setterIn(clasz.symbol)
           if g.isPublic && !(f.symbol.name startsWith "$")
          ) {
             // inserting $outer breaks the bean
             fieldList = javaName(f.symbol) :: javaName(g) :: (if (s != NoSymbol) javaName(s) else null) :: fieldList
      }

      val methodList: List[String] =
	     for (m <- clasz.methods
	          if !m.symbol.isConstructor &&
	          m.symbol.isPublic &&
	          !(m.symbol.name startsWith "$") &&
	          !m.symbol.isGetter &&
	          !m.symbol.isSetter)
       yield javaName(m.symbol)

      // beanInfoClass typestate: entering mode with valid call sequences:
      //   ( visitInnerClass | visitField | visitMethod )* visitEnd

      val constructor = beanInfoClass.visitMethod(
        asm.Opcodes.ACC_PUBLIC,
        INSTANCE_CONSTRUCTOR_NAME,
        mdesc_arglessvoid,
        null, // no java-generic-signature
        EMPTY_STRING_ARRAY // no throwable exceptions
      )

      // constructor typestate: entering mode with valid call sequences:
      //   [ visitAnnotationDefault ] ( visitAnnotation | visitParameterAnnotation | visitAttribute )*

      val stringArrayJType: asm.Type = javaArrayType(JAVA_LANG_STRING)
      val conJType: asm.Type =
        asm.Type.getMethodType(
          asm.Type.VOID_TYPE,
          Array(javaType(ClassClass), stringArrayJType, stringArrayJType): _*
        )

      def push(lst: List[String]) {
        var fi = 0
        for (f <- lst) {
          constructor.visitInsn(asm.Opcodes.DUP)
          constructor.visitLdcInsn(new java.lang.Integer(fi))
          if (f == null) { constructor.visitInsn(asm.Opcodes.ACONST_NULL) }
          else           { constructor.visitLdcInsn(f) }
          constructor.visitInsn(JAVA_LANG_STRING.getOpcode(asm.Opcodes.IASTORE))
          fi += 1
        }
      }

      // constructor typestate: entering mode with valid call sequences:
      //   [ visitCode ( visitFrame | visitXInsn | visitLabel | visitTryCatchBlock | visitLocalVariable | visitLineNumber )* visitMaxs ] visitEnd

      constructor.visitCode()

      constructor.visitVarInsn(asm.Opcodes.ALOAD, 0)
      // push the class
      constructor.visitLdcInsn(javaType(clasz.symbol))

      // push the string array of field information
      constructor.visitLdcInsn(new java.lang.Integer(fieldList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, JAVA_LANG_STRING.getInternalName)
      push(fieldList)

      // push the string array of method information
      constructor.visitLdcInsn(new java.lang.Integer(methodList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, JAVA_LANG_STRING.getInternalName)
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      constructor.visitMethodInsn(asm.Opcodes.INVOKESPECIAL, "scala/beans/ScalaBeanInfo", INSTANCE_CONSTRUCTOR_NAME, conJType.getDescriptor, false)
      constructor.visitInsn(asm.Opcodes.RETURN)

      constructor.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      constructor.visitEnd()

      addInnerClasses(clasz.symbol, beanInfoClass)
      beanInfoClass.visitEnd()

      writeIfNotTooBig("BeanInfo ", beanInfoName, beanInfoClass, clasz.symbol)
    }

  } // end of class JBeanInfoBuilder

  /** A namespace for utilities to normalize the code of an IMethod, over and beyond what IMethod.normalize() strives for.
   * In particular, IMethod.normalize() doesn't collapseJumpChains().
   *
   * TODO Eventually, these utilities should be moved to IMethod and reused from normalize() (there's nothing JVM-specific about them).
   */
  object newNormal {
    /**
     * True if a block is "jump only" which is defined
     * as being a block that consists only of 0 or more instructions that
     * won't make it to the JVM followed by a JUMP.
     */
    def isJumpOnly(b: BasicBlock): Boolean = {
      val nonICode = firstNonIcodeOnlyInstructions(b)
      // by definition a block has to have a jump, conditional jump, return, or throw
      assert(nonICode.hasNext, "empty block")
      nonICode.next.isInstanceOf[JUMP]
    }

    /**
     * Returns the list of instructions in a block that follow all ICode only instructions,
     * where an ICode only instruction is one that won't make it to the JVM
     */
    private def firstNonIcodeOnlyInstructions(b: BasicBlock): Iterator[Instruction] = {
	  def isICodeOnlyInstruction(i: Instruction) = i match {
	    case LOAD_EXCEPTION(_) | SCOPE_ENTER(_) | SCOPE_EXIT(_) => true
	    case _ => false
	  }
	  b.iterator dropWhile isICodeOnlyInstruction
    }

    /**
     * Returns the target of a block that is "jump only" which is defined
     * as being a block that consists only of 0 or more instructions that
     * won't make it to the JVM followed by a JUMP.
     *
     * @param b The basic block to examine
     * @return Some(target) if b is a "jump only" block or None if it's not
     */
    private def getJumpOnlyTarget(b: BasicBlock): Option[BasicBlock] = {
      val nonICode = firstNonIcodeOnlyInstructions(b)
              // by definition a block has to have a jump, conditional jump, return, or throw
      assert(nonICode.nonEmpty, "empty block")
      nonICode.next match {
        case JUMP(whereto) =>
          assert(!nonICode.hasNext, "A block contains instructions after JUMP (looks like enterIgnoreMode() was itself ignored.)")
          Some(whereto)
        case _ => None
      }
    }

    /**
     * Collapse a chain of "jump-only" blocks such as:
     *
     *      JUMP b1;
     *  b1: JUMP b2;
     *  b2: JUMP ... etc.
     *
     *  by re-wiring predecessors to target directly the "final destination".
     *  Even if covered by an exception handler, a "non-self-loop jump-only block" can always be removed.

     *  Returns true if any replacement was made, false otherwise.
     *
     *  In more detail:
     *    Starting at each of the entry points (m.startBlock, the start block of each exception handler)
     *    rephrase those control-flow instructions targeting a jump-only block (which jumps to a final destination D) to target D.
     *    The blocks thus skipped become eligible to removed by the reachability analyzer
     *
     *  Rationale for this normalization:
     *    test/files/run/private-inline.scala after -optimize is chock full of
     *    BasicBlocks containing just JUMP(whereto), where no exception handler straddles them.
     *    They should be collapsed by IMethod.normalize() but aren't.
     *    That was fine in FJBG times when by the time the exception table was emitted,
     *    it already contained "anchored" labels (ie instruction offsets were known)
     *    and thus ranges with identical (start, end) (i.e, identical after GenJVM omitted the JUMPs in question)
     *    could be weeded out to avoid "java.lang.ClassFormatError: Illegal exception table range"
     *    Now that visitTryCatchBlock() must be called before Labels are resolved,
     *    renders the BasicBlocks described above (to recap, consisting of just a JUMP) unreachable.
     */
    private def collapseJumpOnlyBlocks(m: IMethod) {
      assert(m.hasCode, "code-less method")

      def rephraseGotos(detour: mutable.Map[BasicBlock, BasicBlock]) {
        def lookup(b: BasicBlock) = detour.getOrElse(b, b)

        m.code.startBlock = lookup(m.code.startBlock)

        for(eh <- m.exh)
          eh.setStartBlock(lookup(eh.startBlock))

        for (b <- m.blocks) {
          def replaceLastInstruction(i: Instruction) = {
            if (b.lastInstruction != i) {
              val idxLast = b.size - 1
	          debuglog(s"In block $b, replacing last instruction ${b.lastInstruction} with ${i}")
	          b.replaceInstruction(idxLast, i)
            }
          }

          b.lastInstruction match {
            case JUMP(whereto) =>
              replaceLastInstruction(JUMP(lookup(whereto)))
            case CJUMP(succ, fail, cond, kind) =>
              replaceLastInstruction(CJUMP(lookup(succ), lookup(fail), cond, kind))
            case CZJUMP(succ, fail, cond, kind)  =>
              replaceLastInstruction(CZJUMP(lookup(succ), lookup(fail), cond, kind))
            case SWITCH(tags, labels) =>
              val newLabels = (labels map lookup)
              replaceLastInstruction(SWITCH(tags, newLabels))
            case _ => ()
          }
        }
      }

      /*
       * Computes a mapping from jump only block to its
       * final destination which is either a non-jump-only
       * block or, if it's in a jump-only block cycle, is
       * itself
       */
      def computeDetour: mutable.Map[BasicBlock, BasicBlock] = {
        // fetch the jump only blocks and their immediate destinations
        val pairs = for {
          block <- m.blocks.toIterator
          target <- getJumpOnlyTarget(block)
        } yield(block, target)

        // mapping from a jump-only block to our current knowledge of its
        // final destination. Initially it's just jump block to immediate jump
        // target
        val detour = mutable.Map[BasicBlock, BasicBlock](pairs.toSeq:_*)

        // for each jump-only block find its final destination
        // taking advantage of the destinations we found for previous
        // blocks
        for (key <- detour.keySet) {
          // we use the Robert Floyd's classic Tortoise and Hare algorithm
          @tailrec
          def findDestination(tortoise: BasicBlock, hare: BasicBlock): BasicBlock = {
            if (tortoise == hare)
              // cycle detected, map key to key
              key
            else if (detour contains hare) {
              // advance hare once
              val hare1 = detour(hare)
              // make sure we can advance hare a second time
              if (detour contains hare1)
                // advance tortoise once and hare a second time
                findDestination(detour(tortoise), detour(hare1))
              else
                // hare1 is not in the map so it's not a jump-only block, it's the destination
                hare1
            } else
              // hare is not in the map so it's not a jump-only block, it's the destination
              hare
          }
          // update the mapping for key based on its final destination
          detour(key) = findDestination(key, detour(key))
        }
        detour
      }

      val detour = computeDetour
      rephraseGotos(detour)

      if (settings.debug) {
        val (remappings, cycles) = detour partition {case (source, target) => source != target}
        for ((source, target) <- remappings) {
		   debuglog(s"Will elide jump only block $source because it can be jumped around to get to $target.")
                   if (m.startBlock == source) devWarning("startBlock should have been re-wired by now")
        }
        val sources = remappings.keySet
        val targets = remappings.values.toSet
        val intersection = sources intersect targets

        if (intersection.nonEmpty) devWarning(s"contradiction: we seem to have some source and target overlap in blocks ${intersection.mkString}. Map was ${detour.mkString}")

        for ((source, _) <- cycles) {
          debuglog(s"Block $source is in a do-nothing infinite loop. Did the user write 'while(true){}'?")
        }
      }
    }

    /**
     * Removes all blocks that are unreachable in a method using a standard reachability analysis.
     */
    def elimUnreachableBlocks(m: IMethod) {
      assert(m.hasCode, "code-less method")

      // assume nothing is reachable until we prove it can be reached
      val reachable = mutable.Set[BasicBlock]()

      // the set of blocks that we know are reachable but have
      // yet to be  marked reachable, initially only the start block
      val worklist = mutable.Set(m.startBlock)

      while (worklist.nonEmpty) {
        val block = worklist.head
        worklist remove block
        // we know that one is reachable
        reachable add block
        // so are its successors, so go back around and add the ones we still
        // think are unreachable
        worklist ++= (block.successors filterNot reachable)
      }

      // exception handlers need to be told not to cover unreachable blocks
      // and exception handlers that no longer cover any blocks need to be
      // removed entirely
      val unusedExceptionHandlers = mutable.Set[ExceptionHandler]()
      for (exh <- m.exh) {
        exh.covered = exh.covered filter reachable
        if (exh.covered.isEmpty) {
          unusedExceptionHandlers += exh
        }
      }

      // remove the unused exception handler references
      if (settings.debug)
        for (exh <- unusedExceptionHandlers) debuglog(s"eliding exception handler $exh because it does not cover any reachable blocks")
      m.exh = m.exh filterNot unusedExceptionHandlers

      // everything not in the reachable set is unreachable, unused, and unloved. buh bye
      for (b <- m.blocks filterNot reachable) {
    	  debuglog(s"eliding block $b because it is unreachable")
    	  m.code removeBlock b
      }
    }

    def normalize(m: IMethod) {
      if(!m.hasCode) { return }
      collapseJumpOnlyBlocks(m)
      if (settings.optimise)
        elimUnreachableBlocks(m)
      icodes checkValid m
    }

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
       settings.Ynogenericsig
    || sym.isArtifact
    || sym.isLiftedMethod
    || sym.isBridge
    || (sym.ownerChain exists (_.isImplClass))
  )

  final def staticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol, unit: CompilationUnit): String = {
    if (sym.isDeferred) null // only add generic signature if method concrete; bug #1745
    else {
      // SI-3452 Static forwarder generation uses the same erased signature as the method if forwards to.
      // By rights, it should use the signature as-seen-from the module class, and add suitable
      // primitive and value-class boxing/unboxing.
      // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
      // (one that doesn't erase to the actual signature). See run/t3452b for a test case.
      val memberTpe = enteringErasure(moduleClass.thisType.memberInfo(sym))
      val erasedMemberType = erasure.erasure(sym)(memberTpe)
      if (erasedMemberType =:= sym.info)
        getGenericSignature(sym, moduleClass, memberTpe, unit)
      else null
    }
  }

  /** @return
   *   - `null` if no Java signature is to be added (`null` is what ASM expects in these cases).
   *   - otherwise the signature in question
   */
  def getGenericSignature(sym: Symbol, owner: Symbol, unit: CompilationUnit): String = {
    val memberTpe = enteringErasure(owner.thisType.memberInfo(sym))
    getGenericSignature(sym, owner, memberTpe, unit)
  }
  def getGenericSignature(sym: Symbol, owner: Symbol, memberTpe: Type, unit: CompilationUnit): String = {
    if (!needsGenericSignature(sym)) { return null }

    val jsOpt: Option[String] = erasure.javaSig(sym, memberTpe)
    if (jsOpt.isEmpty) { return null }

    val sig = jsOpt.get
    log(sig) // This seems useful enough in the general case.

        def wrap(op: => Unit) = {
          try   { op; true }
          catch { case _: Throwable => false }
        }

    if (settings.Xverify) {
      // Run the signature parser to catch bogus signatures.
      val isValidSignature = wrap {
        // Alternative: scala.tools.reflect.SigParser (frontend to sun.reflect.generics.parser.SignatureParser)
        import scala.tools.asm.util.CheckClassAdapter
        if (sym.isMethod)    { CheckClassAdapter checkMethodSignature sig } // requires asm-util.jar
        else if (sym.isTerm) { CheckClassAdapter checkFieldSignature  sig }
        else                 { CheckClassAdapter checkClassSignature  sig }
      }

      if(!isValidSignature) {
        reporter.warning(sym.pos,
            """|compiler bug: created invalid generic signature for %s in %s
               |signature: %s
               |if this is reproducible, please report bug at https://issues.scala-lang.org/
            """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig))
        return null
      }
    }

    if ((settings.check containsName phaseName)) {
      val normalizedTpe = enteringErasure(erasure.prepareSigMap(memberTpe))
      val bytecodeTpe = owner.thisType.memberInfo(sym)
      if (!sym.isType && !sym.isConstructor && !(erasure.erasure(sym)(normalizedTpe) =:= bytecodeTpe)) {
        reporter.warning(sym.pos,
            """|compiler bug: created generic signature for %s in %s that does not conform to its erasure
               |signature: %s
               |original type: %s
               |normalized type: %s
               |erasure type: %s
               |if this is reproducible, please report bug at http://issues.scala-lang.org/
            """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig, memberTpe, normalizedTpe, bytecodeTpe))
         return null
      }
    }

    sig
  }

  def ubytesToCharArray(bytes: Array[Byte]): Array[Char] = {
    val ca = new Array[Char](bytes.length)
    var idx = 0
    while(idx < bytes.length) {
      val b: Byte = bytes(idx)
      assert((b & ~0x7f) == 0)
      ca(idx) = b.asInstanceOf[Char]
      idx += 1
    }

    ca
  }

  final def arrEncode(sb: ScalaSigBytes): Array[String] = {
    var strs: List[String]  = Nil
    val bSeven: Array[Byte] = sb.sevenBitsMayBeZero
    // chop into slices of at most 65535 bytes, counting 0x00 as taking two bytes (as per JVMS 4.4.7 The CONSTANT_Utf8_info Structure)
    var prevOffset = 0
    var offset     = 0
    var encLength  = 0
    while(offset < bSeven.length) {
      val deltaEncLength = (if(bSeven(offset) == 0) 2 else 1)
      val newEncLength = encLength.toLong + deltaEncLength
      if(newEncLength >= 65535) {
        val ba     = bSeven.slice(prevOffset, offset)
        strs     ::= new java.lang.String(ubytesToCharArray(ba))
        encLength  = 0
        prevOffset = offset
      } else {
        encLength += deltaEncLength
        offset    += 1
      }
    }
    if(prevOffset < offset) {
      assert(offset == bSeven.length)
      val ba = bSeven.slice(prevOffset, offset)
      strs ::= new java.lang.String(ubytesToCharArray(ba))
    }
    assert(strs.size > 1, "encode instead as one String via strEncode()") // TODO too strict?
    strs.reverse.toArray
  }

  private def strEncode(sb: ScalaSigBytes): String = {
    val ca = ubytesToCharArray(sb.sevenBitsMayBeZero)
    new java.lang.String(ca)
    // debug val bvA = new asm.ByteVector; bvA.putUTF8(s)
    // debug val enc: Array[Byte] = scala.reflect.internal.pickling.ByteCodecs.encode(bytes)
    // debug assert(enc(idx) == bvA.getByte(idx + 2))
    // debug assert(bvA.getLength == enc.size + 2)
  }
}
