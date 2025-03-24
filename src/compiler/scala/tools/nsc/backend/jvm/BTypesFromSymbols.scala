/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm

import scala.reflect.internal.Flags.{DEFERRED, SYNTHESIZE_IMPL_IN_SUBCLASS}
import scala.tools.asm
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.BackendReporting._

/**
 * This class mainly contains the method classBTypeFromSymbol, which extracts the necessary
 * information from a symbol and its type to create the corresponding ClassBType. It requires
 * access to the compiler (global parameter).
 */
abstract class BTypesFromSymbols[G <: Global](val global: G) extends BTypes {
  val frontendAccess: PostProcessorFrontendAccess

  import global._
  import definitions._
  import genBCode._
  import codeGen.CodeGenImpl._
  import postProcessor.{bTypesFromClassfile, byteCodeRepository}

  val coreBTypes = new CoreBTypesFromSymbols[G] {
    val bTypes: BTypesFromSymbols.this.type = BTypesFromSymbols.this
  }
  import coreBTypes._

  final def initialize(): Unit = {
    coreBTypes.initialize()
  }

  // helpers that need access to global.
  // TODO @lry create a separate component, they don't belong to BTypesFromSymbols

  final val strMODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString

  private val primitiveCompilationUnits = Set(
    "Unit.scala",
    "Boolean.scala",
    "Char.scala",
    "Byte.scala",
    "Short.scala",
    "Int.scala",
    "Float.scala",
    "Long.scala",
    "Double.scala"
  )

  /**
   * True if the current compilation unit is of a primitive class (scala.Boolean et al).
   * Used only in assertions.
   */
  def isCompilingPrimitive = {
    primitiveCompilationUnits(currentUnit.source.file.name)
  }

  def isCompilingArray = {
    currentUnit.source.file.name == "Array.scala"
  }

  // end helpers

  /**
   * The ClassBType for a class symbol `classSym`.
   *
   * The class symbol scala.Nothing is mapped to the class scala.runtime.Nothing$. Similarly,
   * scala.Null is mapped to scala.runtime.Null$. This is because there exist no class files
   * for the Nothing / Null. If used for example as a parameter type, we use the runtime classes
   * in the classfile method signature.
   */
  final def classBTypeFromSymbol(sym: Symbol): ClassBType = {
    // For each java class, the scala compiler creates a class and a module (thus a module class).
    // If the `sym` is a java module class, we use the java class instead. This ensures that the
    // ClassBType is created from the main class (instead of the module class).
    // The two symbols have the same name, so the resulting internalName is the same.
    // Phase travel (exitingPickler) required for scala/bug#6613 - linkedCoC is only reliable in early phases (nesting)
    val classSym = if (sym.isJavaDefined && sym.isModuleClass) exitingPickler(sym.linkedClassOfClass) else sym

    assert(classSym != NoSymbol, "Cannot create ClassBType from NoSymbol")
    assert(classSym.isClass, s"Cannot create ClassBType from non-class symbol $classSym")
    // note: classSym can be scala.Array, see https://github.com/scala/bug/issues/12225#issuecomment-729687859
    if (global.settings.isDebug) {
      // OPT this assertion has too much performance overhead to run unconditionally
      assert(!primitiveTypeToBType.contains(classSym) || isCompilingPrimitive, s"Cannot create ClassBType for primitive class symbol $classSym")
    }

    if (classSym == NothingClass) srNothingRef
    else if (classSym == NullClass) srNullRef
    else {
      val internalName = classSym.javaBinaryNameString
      // The new ClassBType is added to the map via its apply, before we set its info. This
      // allows initializing cyclic dependencies, see the comment on variable ClassBType._info.
      val btype = ClassBType.apply(internalName, classSym, fromSymbol = true)(classBTypeFromSymbolInit)
      if (currentRun.compiles(classSym))
        assert(btype.fromSymbol, s"ClassBType for class being compiled was already created from a classfile: ${classSym.fullName}")
      btype
    }
  }

  private val classBTypeFromSymbolInit = (res: ClassBType, classSym: Symbol) =>
    if (completeSilentlyAndCheckErroneous(classSym))
      Left(NoClassBTypeInfoClassSymbolInfoFailedSI9111(classSym.fullName))
    else computeClassInfo(classSym, res)

  /**
   * Builds a [[MethodBType]] for a method symbol.
   */
  final def methodBTypeFromSymbol(methodSymbol: Symbol): MethodBType = {
    assert(methodSymbol.isMethod, s"not a method-symbol: $methodSymbol")
    methodBTypeFromMethodType(methodSymbol.info, methodSymbol.isClassConstructor || methodSymbol.isConstructor)
  }

  /**
   * Builds a [[MethodBType]] for a method type.
   */
  final def methodBTypeFromMethodType(tpe: Type, isConstructor: Boolean): MethodBType = {
    val resultType: BType =
      if (isConstructor) UNIT
      else typeToBType(tpe.resultType)
    val params = tpe.params
    // OPT allocation hotspot
    val paramBTypes = BType.newArray(params.length)
    mapToArray(params, paramBTypes, 0)(param => typeToBType(param.tpe))
    MethodBType(paramBTypes, resultType)
  }

  def bootstrapMethodArg(t: Constant, pos: Position): AnyRef = t.value match {
    case mt: Type =>
      transformedType(mt) match {
        case mt1: MethodType =>
          methodBTypeFromMethodType(mt1, isConstructor = false).toASMType
        case transformed =>
          typeToBType(transformed).toASMType
      }
    case sym: Symbol if sym.owner.isJavaDefined && sym.isStaticMember => staticHandleFromSymbol(sym)
    case sym: Symbol => handleFromMethodSymbol(sym)
    case value: String => value
    case value if t.isNonUnitAnyVal => value.asInstanceOf[AnyRef]
    case _ => reporter.error(pos, s"Unable to convert static argument of ApplyDynamic into a classfile constant: $t"); null
  }

  def staticHandleFromSymbol(sym: Symbol): asm.Handle = {
    val descriptor = methodBTypeFromMethodType(sym.info, isConstructor = false).descriptor
    val ownerBType = classBTypeFromSymbol(sym.owner)
    val rawInternalName = ownerBType.internalName
    // object Qux { def foo = 1 } --> we want the handle to be to (static) Qux.foo, not (member) Qux$.foo
    val mustUseMirrorClass = !sym.isJava && sym.owner.isModuleClass && !sym.isStaticMember
    // ... but we don't know that the mirror class exists! (if there's no companion, the class is synthesized in jvm without a symbol)
    val ownerInternalName = if (mustUseMirrorClass) rawInternalName stripSuffix nme.MODULE_SUFFIX_STRING else rawInternalName
    val isInterface = sym.owner.linkedClassOfClass.isTraitOrInterface
    new asm.Handle(asm.Opcodes.H_INVOKESTATIC, ownerInternalName, sym.name.encoded, descriptor, isInterface)
  }

  def handleFromMethodSymbol(sym: Symbol): asm.Handle = {
    val isConstructor = (sym.isClassConstructor)
    val descriptor = methodBTypeFromMethodType(sym.info, isConstructor).descriptor
    val ownerBType = classBTypeFromSymbol(sym.owner)
    val rawInternalName = ownerBType.internalName
    val ownerInternalName = rawInternalName
    val isInterface = sym.owner.isTraitOrInterface
    val tag =
              if (sym.isStaticMember) {
                if (sym.owner.isJavaDefined) throw new UnsupportedOperationException("handled by staticHandleFromSymbol")
                else asm.Opcodes.H_INVOKESTATIC
              } else if (isConstructor) asm.Opcodes.H_NEWINVOKESPECIAL
              else if (isInterface) asm.Opcodes.H_INVOKEINTERFACE
              else asm.Opcodes.H_INVOKEVIRTUAL
    new asm.Handle(tag, ownerInternalName, if (isConstructor) sym.name.toString else sym.name.encoded, descriptor, isInterface)
  }

  /**
   * This method returns the BType for a type reference, for example a parameter type.
   */
  final def typeToBType(t: Type): BType = {
    import definitions.ArrayClass

    /*
     * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
     * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
     */
    def primitiveOrClassToBType(sym: Symbol): BType = {
      assertClassNotArray(sym)
      primitiveTypeToBType.getOrElse(sym, null) match {
        case null => classBTypeFromSymbol(sym)
        case res => res
      }
    }

    /*
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TypeRef for T is replaced by ObjectRef.
     */
    def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
      assert(sym.isType && isCompilingArray, sym)
      ObjectRef
    }

    t.dealiasWiden match {
      case TypeRef(_, ArrayClass, List(arg))  => ArrayBType(typeToBType(arg)) // Array type such as Array[Int] (kept by erasure)
      case TypeRef(_, sym, _) if !sym.isClass => nonClassTypeRefToBType(sym)  // See comment on nonClassTypeRefToBType
      case TypeRef(_, sym, _)                 => primitiveOrClassToBType(sym) // Common reference to a type such as scala.Int or java.lang.String
      case ClassInfoType(_, _, sym)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes typeToBType(moduleClassSymbol.info)

      /* The cases below should probably never occur. They are kept for now to avoid introducing
       * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
       * test suite don't produce any warning.
       */

      case tp =>
        runReporting.warning(
          tp.typeSymbol.pos,
          s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
            "If possible, please file a bug on https://github.com/scala/bug/issues.",
          WarningCategory.Other,
          site = "")

        tp match {
          case ThisType(ArrayClass)    => ObjectRef // was introduced in 9b17332f11 to fix scala/bug#999, but this code is not reached in its test, or any other test
          case ThisType(sym)           => classBTypeFromSymbol(sym)
          case SingleType(_, sym)      => primitiveOrClassToBType(sym)
          case ConstantType(_)         => typeToBType(t.underlying)
          case RefinedType(parents, _) => parents.map(typeToBType(_).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b).get)
          case AnnotatedType(_, at)    => typeToBType(at)
          case ExistentialType(_, et)  => typeToBType(et)
          case x                       => throw new MatchError(x)
        }
    }
  }

  def assertClassNotArray(sym: Symbol): Unit = {
    assert(sym.isClass, sym)
    assert(sym != definitions.ArrayClass || isCompilingArray, sym)
  }

  def implementedInterfaces(classSym: Symbol): List[Symbol] = {

    def isInterfaceOrTrait(sym: Symbol) = sym.isInterface || sym.isTrait

    val classParents = classSym.info.parents

    val minimizedParents = if (classSym.isJavaDefined) classParents else erasure.minimizeParents(classSym, classParents)
    // We keep the superClass when computing minimizeParents to eliminate more interfaces.
    // Example: T can be eliminated from D
    //   trait T
    //   class C extends T
    //   class D extends C with T
    val interfaces = minimizedParents match {
      case superClass :: ifs if !isInterfaceOrTrait(superClass.typeSymbol) =>
        ifs
      case ifs =>
        // minimizeParents removes the superclass if it's redundant, for example:
        //  trait A
        //  class C extends Object with A  // minimizeParents removes Object
        ifs
    }
    interfaces.map(_.typeSymbol)
  }

  /**
   * The member classes of a class symbol. Note that the result of this method depends on the
   * current phase, for example, after lambdalift, all local classes become member of the enclosing
   * class.
   *
   * Specialized classes are always considered top-level, see comment in BTypes.
   */
  private def memberClassesForInnerClassTable(classSymbol: Symbol): List[Symbol] = List.from(classSymbol.info.decls.iterator.collect({
    case sym if sym.isClass && !considerAsTopLevelImplementationArtifact(sym) =>
      sym
    case sym if sym.isModule && !considerAsTopLevelImplementationArtifact(sym) =>
      val r = exitingPickler(sym.moduleClass)
      assert(r != NoSymbol, sym.fullLocationString)
      r
  }))

  private def computeClassInfo(classSym: Symbol, classBType: ClassBType): Right[Nothing, ClassInfo] = {
    /*
     * Reconstruct the classfile flags from a Java defined class symbol.
     *
     * The implementation of this method is slightly different from `javaFlags` in BTypesFromSymbols.
     * The javaFlags method is primarily used to map Scala symbol flags to sensible classfile flags
     * that are used in the generated classfiles. For example, all classes emitted by the Scala
     * compiler have ACC_PUBLIC.
     *
     * When building a [[ClassBType]] from a Java class symbol, the flags in the type's `info` have
     * to correspond exactly to the flags in the classfile. For example, if the class is package
     * protected (i.e., it doesn't have the ACC_PUBLIC flag), this needs to be reflected in the
     * ClassBType. For example, the inliner needs the correct flags for access checks.
     *
     * Class flags are listed here:
     *   https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1-200-E.1
     */
    def javaClassfileFlags(classSym: Symbol): Int = {
      assert(classSym.isJava, s"Expected Java class symbol, got ${classSym.fullName}")
      import asm.Opcodes._
      // scala/bug#9393: the classfile / java source parser make java annotation symbols look like classes.
      // here we recover the actual classfile flags.
      (if (classSym.hasJavaAnnotationFlag) ACC_ANNOTATION | ACC_INTERFACE | ACC_ABSTRACT else 0) |
      (if (classSym.isPublic)              ACC_PUBLIC    else 0) |
      (if (classSym.isFinal)               ACC_FINAL     else 0) |
       // see the link above. javac does the same: ACC_SUPER for all classes, but not interfaces.)
      (if (classSym.isInterface)           ACC_INTERFACE else ACC_SUPER) |
      (if (classSym.hasAbstractFlag)       ACC_ABSTRACT  else 0) |
      (if (classSym.isArtifact)            ACC_SYNTHETIC else 0) |
      (if (classSym.hasJavaEnumFlag)       ACC_ENUM      else 0)
    }

    // Check for hasAnnotationFlag for scala/bug#9393: the classfile / java source parsers add
    // scala.annotation.Annotation as superclass to java annotations. In reality, java
    // annotation classfiles have superclass Object (like any interface classfile).
    val superClassSym = if (classSym.hasJavaAnnotationFlag) ObjectClass else {
      val sc = classSym.superClass
      // scala/bug#9393: Java annotation classes don't have the ABSTRACT/INTERFACE flag, so they appear
      // (wrongly) as superclasses. Fix this for BTypes: the java annotation will appear as interface
      // (handled by method implementedInterfaces), the superclass is set to Object.
      if (sc.hasJavaAnnotationFlag) ObjectClass
      else sc
    }
    assert(
      if (classSym == ObjectClass)
        superClassSym == NoSymbol
      else if (classSym.isInterface)
        superClassSym == ObjectClass
      else
        // A ClassBType for a primitive class (scala.Boolean et al) is only created when compiling these classes.
        ((superClassSym != NoSymbol) && !superClassSym.isInterface) || (isCompilingPrimitive && primitiveTypeToBType.contains(classSym)),
      s"Bad superClass for $classSym: $superClassSym"
    )
    val superClass = if (superClassSym == NoSymbol) None
                     else Some(classBTypeFromSymbol(superClassSym))

    val interfaces = implementedInterfaces(classSym).map(classBTypeFromSymbol)

    val flags = {
      if (classSym.isJava) javaClassfileFlags(classSym) // see comment on javaClassfileFlags
      else javaFlags(classSym)
    }

    /* The InnerClass table of a class C must contain all nested classes of C, even if they are only
     * declared but not otherwise referenced in C (from the bytecode or a method / field signature).
     * We collect them here.
     */
    lazy val nestedClassSymbols = {
      val linkedClass = exitingPickler(classSym.linkedClassOfClass) // linkedCoC does not work properly in late phases

      // The lambdalift phase lifts all nested classes to the enclosing class, so if we collect
      // member classes right after lambdalift, we obtain all nested classes, including local and
      // anonymous ones.
      val nestedClasses = {
        val allNested = exitingPhase(currentRun.lambdaliftPhase)(memberClassesForInnerClassTable(classSym))
        val nested = {
          // Classes nested in value classes are nested in the companion at this point. For InnerClass /
          // EnclosingMethod, we use the value class as the outer class. So we remove nested classes
          // from the companion that were originally nested in the value class.
          if (exitingPickler(linkedClass.isDerivedValueClass)) allNested.filterNot(classOriginallyNestedInClass(_, linkedClass))
          else allNested
        }

        if (isTopLevelModuleClass(classSym)) {
          // For Java compatibility, member classes of top-level objects are treated as members of
          // the top-level companion class, see comment below.
          val members = exitingPickler(memberClassesForInnerClassTable(classSym))
          nested diff members
        } else {
          nested
        }
      }

      val companionModuleMembers = if (considerAsTopLevelImplementationArtifact(classSym)) Nil else {
        // If this is a top-level class, the member classes of the companion object are added as
        // members of the class. For example:
        //   class C { }
        //   object C {
        //     class D
        //     def f = { class E }
        //   }
        // The class D is added as a member of class C. The reason is: for Java compatibility, the
        // InnerClass attribute for D has "C" (NOT the module class "C$") as the outer class of D
        // (done by buildNestedInfo). See comment in BTypes.
        // For consistency, the InnerClass entry for D needs to be present in C - to Java it looks
        // like D is a member of C, not C$.
        val javaCompatMembers = {
          if (linkedClass != NoSymbol && isTopLevelModuleClass(linkedClass))
          // phase travel to exitingPickler: this makes sure that memberClassesForInnerClassTable only sees member
          // classes, not local classes of the companion module (E in the example) that were lifted by lambdalift.
            exitingPickler(memberClassesForInnerClassTable(linkedClass))
          else
            Nil
        }

        // Classes nested in value classes are nested in the companion at this point. For InnerClass /
        // EnclosingMethod we use the value class as enclosing class. Here we search nested classes
        // in the companion that were originally nested in the value class, and we add them as nested
        // in the value class.
        val valueClassCompanionMembers = {
          if (linkedClass != NoSymbol && exitingPickler(classSym.isDerivedValueClass)) {
            val moduleMemberClasses = exitingPhase(currentRun.lambdaliftPhase)(memberClassesForInnerClassTable(linkedClass))
            moduleMemberClasses.filter(classOriginallyNestedInClass(_, classSym))
          } else
            Nil
        }

        javaCompatMembers ++ valueClassCompanionMembers
      }

      nestedClasses ++ companionModuleMembers
    }

    /*
     * For nested java classes, the scala compiler creates both a class and a module (and therefore
     * a module class) symbol. For example, in `class A { class B {} }`, the nestedClassSymbols
     * for A contain both the class B and the module class B.
     * Here we get rid of the module class B, making sure that the class B is present.
     */
    def nestedClassSymbolsNoJavaModuleClasses = nestedClassSymbols.filter { s =>
      val ok = !(s.isJavaDefined && s.isModuleClass) && !s.hasPackageFlag
      if (!ok)
        if (!s.hasPackageFlag) {
          // We could also search in nestedClassSymbols for s.linkedClassOfClass, but sometimes that
          // returns NoSymbol, so it doesn't work.
          val nb = nestedClassSymbols.count(mc => mc.name == s.name && mc.owner == s.owner)
          assert(nb == 2, s"Java member module without member class: $s - $nestedClassSymbols")
        }
      ok
    }

    val shouldBeLazy = classSym.isJavaDefined || !currentRun.compiles(classSym)

    //cant use Lazy.withLockOrEager or simply encapsulate in Lazy as it is in BTypes so unaware of Phase & Global
    //consider refactor if this get more widely used
    val nestedClasses = if (shouldBeLazy) {
      val ph = phase
      Lazy.withLock(enteringPhase(ph)(nestedClassSymbolsNoJavaModuleClasses.map(classBTypeFromSymbol)))
    } else Lazy.eager(nestedClassSymbolsNoJavaModuleClasses.map(classBTypeFromSymbol))

    val nestedInfo = {
      if (isEmptyNestedInfo(classSym)) Lazy.eagerNone
      else if (shouldBeLazy) {
        val ph = phase
        Lazy.withLock(enteringPhase(ph)(buildNonEmptyNestedInfo(classSym)))
      } else Lazy.eager(buildNonEmptyNestedInfo(classSym))
    }

    val inlineInfo = buildInlineInfo(classSym, classBType.internalName)

    Right(ClassInfo(superClass, interfaces, flags, nestedClasses, nestedInfo, inlineInfo))
  }
  private def isEmptyNestedInfo(innerClassSym: Symbol): Boolean = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")

    val isTopLevel = innerClassSym.rawowner.isPackageClass
    // specialized classes are considered top-level, see comment in BTypes
    if (isTopLevel || considerAsTopLevelImplementationArtifact(innerClassSym)) true
    else if (innerClassSym.rawowner.isTerm) {
      // This case should never be reached: the lambdalift phase mutates the rawowner field of all
      // classes to be the enclosing class. scala/bug#9392 shows an errant macro that leaves a reference
      // to a local class symbol that no longer exists, which is not updated by lambdalift.
      devWarning(innerClassSym.pos,
        s"""The class symbol $innerClassSym with the term symbol ${innerClassSym.rawowner} as `rawowner` reached the backend.
           |Most likely this indicates a stale reference to a non-existing class introduced by a macro, see scala/bug#9392.""".stripMargin)
      true
    } else false
  }
  private def buildNonEmptyNestedInfo(innerClassSym: Symbol): Option[NestedInfo] = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")
    // See comment in BTypes, when is a class marked static in the InnerClass table.
    val isStaticNestedClass = isOriginallyStaticOwner(innerClassSym.originalOwner)

    // After lambdalift (which is where we are), the rawowner field contains the enclosing class.
    val enclosingClass = {
      // (1) Example java source: class C { static class D { } }
      // The Scala compiler creates a class and a module symbol for C. Because D is a static
      // nested class, the symbol for D is nested in the module class C (not in the class C).
      // For the InnerClass attribute, we use the class symbol C, which represents the situation
      // in the source code.

      // (2) Java compatibility. See the big comment in BTypes that summarizes the InnerClass spec.
      if ((innerClassSym.isJavaDefined && innerClassSym.rawowner.isModuleClass) || // (1)
        (!isAnonymousOrLocalClass(innerClassSym) && isTopLevelModuleClass(innerClassSym.rawowner))) { // (2)
        // phase travel for linkedCoC - does not always work in late phases
        exitingPickler(innerClassSym.rawowner.linkedClassOfClass) match {
          case NoSymbol =>
            // For top-level modules without a companion class, see doc of mirrorClassClassBType.
            mirrorClassClassBType(exitingPickler(innerClassSym.rawowner))

          case companionClass =>
            classBTypeFromSymbol(companionClass)
        }
      } else {
        classBTypeFromSymbol(innerClassSym.rawowner)
      }
    }

    val outerName: Option[String] = {
      if (isAnonymousOrLocalClass(innerClassSym)) None
      else Some(enclosingClass.internalName)
    }

    val innerName: Option[String] = {
      // phase travel necessary: after flatten, the name includes the name of outer classes.
      // if some outer name contains $anon, a non-anon class is considered anon.
      if (exitingPickler(innerClassSym.isAnonymousClass || innerClassSym.isAnonymousFunction)) None
      else Some(s"${innerClassSym.rawname}${innerClassSym.moduleSuffix}") // moduleSuffix for module classes
    }

    Some(NestedInfo(enclosingClass, outerName, innerName, isStaticNestedClass, enteringTyper(innerClassSym.isPrivate)))
  }

  /**
   * Build the InlineInfo for a ClassBType from the class symbol.
   *
   * Note that the InlineInfo is only built from the symbolic information for classes that are being
   * compiled. For all other classes we delegate to inlineInfoFromClassfile. The reason is that
   * mixed-in methods are only added to class symbols being compiled, but not to other classes
   * extending traits. Creating the InlineInfo from the symbol would prevent these mixins from being
   * inlined.
   *
   * So for classes being compiled, the InlineInfo is created here and stored in the ScalaInlineInfo
   * classfile attribute.
   */
  private def buildInlineInfo(classSym: Symbol, internalName: InternalName): InlineInfo = {
    // phase travel required, see implementation of `compiles`. for nested classes, it checks if the
    // enclosingTopLevelClass is being compiled. after flatten, all classes are considered top-level,
    // so `compiles` would return `false`.
    if (exitingPickler(currentRun.compiles(classSym))) buildInlineInfoFromClassSymbol(classSym) // InlineInfo required for classes being compiled, we have to create the classfile attribute
    else if (!settings.optInlinerEnabled) BTypes.EmptyInlineInfo // For other classes, we need the InlineInfo only if the inliner is enabled
    else {
      // For classes not being compiled, the InlineInfo is read from the classfile attribute. This
      // fixes an issue with mixed-in methods: the mixin phase enters mixin methods only to class
      // symbols being compiled. For non-compiled classes, we could not build MethodInlineInfos
      // for those mixin members, which prevents inlining.
      byteCodeRepository.classNode(internalName) match {
        case Right(classNode) =>
          bTypesFromClassfile.inlineInfoFromClassfile(classNode)
        case Left(missingClass) =>
          EmptyInlineInfo.copy(warning = Some(ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass)))
      }
    }
  }

  /**
   * Build the [[scala.tools.nsc.backend.jvm.BTypes.InlineInfo]] for a class symbol.
   */
  def buildInlineInfoFromClassSymbol(classSym: Symbol): InlineInfo = {
    val isEffectivelyFinal = classSym.isEffectivelyFinal

    val sam = {
      val considerSam = !classSym.isEffectivelyFinal && {
        isFunctionSymbol(classSym) || classSym.hasAnnotation(FunctionalInterfaceClass)
      }
      if (!considerSam) None
      else {
        // Phase travel necessary. For example, nullary methods (getter of an abstract val) get an
        // empty parameter list in uncurry and would therefore be picked as SAM.
        // Similarly, the fields phases adds abstract trait setters, which should not be considered
        // abstract for SAMs (they do disqualify the SAM from LMF treatment,
        // but an anonymous subclass can be spun up by scalac after making just the single abstract method concrete)
        val samSym = exitingPickler(definitions.samOf(classSym.tpe))
        if (samSym == NoSymbol) None
        else Some(samSym.javaSimpleName.toString + methodBTypeFromSymbol(samSym).descriptor)
      }
    }

    var warning = Option.empty[ClassSymbolInfoFailureSI9111]

    def keepMember(sym: Symbol) = sym.isMethod && !scalaPrimitives.isPrimitive(sym)
    val classMethods = classSym.info.decls.iterator.filter(keepMember)
    val methods = if (!classSym.isJavaDefined) classMethods else {
      // Phase travel important for nested classes (scala-dev#402). When a java class symbol A$B
      // is compiled from source, this ensures that `companionModule` doesn't return the `A$B`
      // symbol created for the `A$B.class` file on the classpath, which might be different.
      val companion = exitingPickler(classSym.companionModule)
      val staticMethods = companion.info.decls.iterator.filter(m => !m.isConstructor && keepMember(m))
      staticMethods ++ classMethods
    }

    // Primitive methods cannot be inlined, so there's no point in building a MethodInlineInfo. Also, some
    // primitive methods (e.g., `isInstanceOf`) have non-erased types, which confuses [[typeToBType]].
    val methodInlineInfos = new collection.mutable.TreeMap[(String, String), MethodInlineInfo]()
    methods.foreach {
      methodSym =>
        if (completeSilentlyAndCheckErroneous(methodSym)) {
          // Happens due to scala/bug#9111. Just don't provide any MethodInlineInfo for that method, we don't need fail the compiler.
          if (!classSym.isJavaDefined) devWarning("scala/bug#9111 should only be possible for Java classes")
          warning = Some(ClassSymbolInfoFailureSI9111(classSym.fullName))
        } else {
          val name = methodSym.javaSimpleName.toString // same as in genDefDef
          val signature = (name, methodBTypeFromSymbol(methodSym).descriptor)

          // In `trait T { object O }`, `oSym.isEffectivelyFinalOrNotOverridden` is true, but the
          // method is abstract in bytecode, `defDef.rhs.isEmpty`. Abstract methods are excluded
          // so they are not marked final in the InlineInfo attribute.
          //
          // However, due to https://github.com/scala/scala-dev/issues/126, this currently does not
          // work, the abstract accessor for O will be marked effectivelyFinal.
          val effectivelyFinal = methodSym.isEffectivelyFinalOrNotOverridden && !(methodSym hasFlag DEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS)

          val info = MethodInlineInfo(
            effectivelyFinal = effectivelyFinal,
            annotatedInline = methodSym.hasAnnotation(ScalaInlineClass),
            annotatedNoInline = methodSym.hasAnnotation(ScalaNoInlineClass))

          if (needsStaticImplMethod(methodSym)) {
            val staticName = traitSuperAccessorName(methodSym)
            val selfParam = methodSym.newSyntheticValueParam(methodSym.owner.typeConstructor, nme.SELF)
            val staticMethodType = methodSym.info match {
              case mt@MethodType(params, res) => copyMethodType(mt, selfParam :: params, res)
              case x                          => throw new MatchError(x)
            }
            val staticMethodSignature = (staticName, methodBTypeFromMethodType(staticMethodType, isConstructor = false).descriptor)
            val staticMethodInfo = MethodInlineInfo(
              effectivelyFinal = true,
              annotatedInline = info.annotatedInline,
              annotatedNoInline = info.annotatedNoInline)
            methodInlineInfos(staticMethodSignature) = staticMethodInfo
            if (!methodSym.isMixinConstructor)
              methodInlineInfos(signature) = info
          } else
            methodInlineInfos(signature) = info
        }
    }

    InlineInfo(isEffectivelyFinal, sam, methodInlineInfos, warning)
  }

  /**
   * For top-level objects without a companion class, the compiler generates a mirror class with
   * static forwarders (Java compat). There's no symbol for the mirror class, but we still need a
   * ClassBType (its info.nestedClasses will hold the InnerClass entries, see comment in BTypes).
   */
  def mirrorClassClassBType(moduleClassSym: Symbol): ClassBType = {
    assert(isTopLevelModuleClass(moduleClassSym), s"not a top-level module class: $moduleClassSym")
    val internalName = moduleClassSym.javaBinaryNameString.stripSuffix(nme.MODULE_SUFFIX_STRING)
    ClassBType(internalName, moduleClassSym, fromSymbol = true) { (_: ClassBType, moduleClassSym) =>
      val shouldBeLazy = moduleClassSym.isJavaDefined || !currentRun.compiles(moduleClassSym)
      val nested = Lazy.withLockOrEager(shouldBeLazy, exitingPickler(memberClassesForInnerClassTable(moduleClassSym)) map classBTypeFromSymbol)
      Right(ClassInfo(
        superClass = Some(ObjectRef),
        interfaces = Nil,
        flags = asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL,
        nestedClasses = nested,
        nestedInfo = Lazy.eagerNone,
        inlineInfo = EmptyInlineInfo.copy(isEffectivelyFinal = true))) // no method inline infos needed, scala never invokes methods on the mirror class
    }
  }

  /**
   * True for module classes of package level objects. The backend will generate a mirror class for
   * such objects.
   */
  final def isTopLevelModuleClass(sym: Symbol): Boolean = exitingPickler {
    // phase travel to pickler required for isNestedClass (looks at owner)
    sym.isModuleClass && !sym.isNestedClass
  }

  /**
   * True for module classes of modules that are top-level or owned only by objects. Module classes
   * for such objects will get a MODULE$ field and a corresponding static initializer.
   */
  final def isStaticModuleClass(sym: Symbol): Boolean = {
    sym.isModuleClass &&
    isOriginallyStaticOwner(sym.originalOwner) // isStaticModuleClass is a source-level property, see comment on isOriginallyStaticOwner
  }


  /**
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   * for all:
   *  - deprecated
   *
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   */
  final def javaFlags(sym: Symbol): Int = {
    // constructors of module classes should be private. introduced in b06edbc, probably to prevent
    // creating module instances from java. for nested modules, the constructor needs to be public
    // since they are created by the outer class and stored in a field. a java client can create
    // new instances via outerClassInstance.new InnerModuleClass$().
    // TODO: do this early, mark the symbol private.
    val privateFlag =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModuleClass(sym.owner))

    // Symbols marked in source as `final` have the FINAL flag. (In the past, the flag was also
    // added to modules and module classes, not anymore since 296b706).
    // Note that the presence of the `FINAL` flag on a symbol does not correspond 1:1 to emitting
    // ACC_FINAL in bytecode.
    //
    // Top-level modules are marked ACC_FINAL in bytecode (even without the FINAL flag).
    // Currently, nested objects don't get the flag (originally, to allow overriding under the now-removed -Yoverride-objects, scala/bug#5676).
    // TODO: give nested objects the ACC_FINAL flag again, since we won't let them be overridden
    //
    // For fields, only eager val fields can receive ACC_FINAL. vars or lazy vals can't:
    // Source: https://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
    // "Another problem is that the specification allows aggressive
    // optimization of final fields. Within a thread, it is permissible to
    // reorder reads of a final field with those modifications of a final
    // field that do not take place in the constructor."
    //
    // A var or lazy val which is marked final still has meaning to the
    // scala compiler. The word final is heavily overloaded unfortunately;
    // for us it means "not overridable". At present you can't override
    // vars regardless; this may change.

    val finalFlag = (
           (sym.isFinal || isTopLevelModuleClass(sym))
        && !sym.enclClass.isTrait
        && !sym.isClassConstructor
        && (!sym.isMutable || nme.isTraitSetterName(sym.name)) // lazy vals and vars and their setters cannot be final, but trait setters are
      )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract is present.
    import asm.Opcodes._
      ( if (privateFlag) ACC_PRIVATE else ACC_PUBLIC) |
        ( if ((sym.isDeferred && !sym.hasFlag(symtab.Flags.JAVA_DEFAULTMETHOD))|| sym.hasAbstractFlag) ACC_ABSTRACT else 0) |
        ( if (sym.isTraitOrInterface) ACC_INTERFACE else 0) |
        ( if (finalFlag && !sym.hasAbstractFlag) ACC_FINAL else 0) |
        ( if (sym.isStaticMember) ACC_STATIC else 0) |
        ( if (sym.isBridge) ACC_BRIDGE | ACC_SYNTHETIC else 0) |
        ( if (sym.isArtifact) ACC_SYNTHETIC else 0) |
        ( if (sym.isClass && !sym.isTraitOrInterface) ACC_SUPER else 0) |
        ( if (sym.hasJavaEnumFlag) ACC_ENUM else 0) |
        ( if (sym.isVarargsMethod) ACC_VARARGS else 0) |
        ( if (sym.hasFlag(symtab.Flags.SYNCHRONIZED)) ACC_SYNCHRONIZED else 0) |
        ( if (sym.isDeprecated) asm.Opcodes.ACC_DEPRECATED else 0)
  }

  def javaFieldFlags(sym: Symbol) = {
    javaFlags(sym) |
      ( if (sym hasAnnotation TransientAttr) asm.Opcodes.ACC_TRANSIENT else 0) |
      ( if (sym hasAnnotation VolatileAttr)  asm.Opcodes.ACC_VOLATILE  else 0) |
      ( if (sym.isMutable) 0 else asm.Opcodes.ACC_FINAL)
  }
}
