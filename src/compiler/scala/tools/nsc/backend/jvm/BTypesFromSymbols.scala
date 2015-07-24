/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import scala.tools.nsc.backend.jvm.opt._
import scala.tools.nsc.backend.jvm.BTypes.{InlineInfo, MethodInlineInfo, InternalName}
import BackendReporting._
import scala.tools.nsc.settings.ScalaSettings

/**
 * This class mainly contains the method classBTypeFromSymbol, which extracts the necessary
 * information from a symbol and its type to create the corresponding ClassBType. It requires
 * access to the compiler (global parameter).
 *
 * The mixin CoreBTypes defines core BTypes that are used in the backend. Building these BTypes
 * uses classBTypeFromSymbol, hence requires access to the compiler (global).
 *
 * BTypesFromSymbols extends BTypes because the implementation of BTypes requires access to some
 * of the core btypes. They are declared in BTypes as abstract members. Note that BTypes does
 * not have access to the compiler instance.
 */
class BTypesFromSymbols[G <: Global](val global: G) extends BTypes {
  import global._
  import definitions._

  val bCodeICodeCommon: BCodeICodeCommon[global.type] = new BCodeICodeCommon(global)
  val bCodeAsmCommon: BCodeAsmCommon[global.type] = new BCodeAsmCommon(global)
  import bCodeAsmCommon._

  // Why the proxy, see documentation of class [[CoreBTypes]].
  val coreBTypes = new CoreBTypesProxy[this.type](this)
  import coreBTypes._

  val byteCodeRepository = new ByteCodeRepository(global.classPath, javaDefinedClasses, recordPerRunCache(collection.concurrent.TrieMap.empty))

  val localOpt: LocalOpt[this.type] = new LocalOpt(this)

  val inliner: Inliner[this.type] = new Inliner(this)

  val closureOptimizer: ClosureOptimizer[this.type] = new ClosureOptimizer(this)

  val callGraph: CallGraph[this.type] = new CallGraph(this)

  val backendReporting: BackendReporting = new BackendReportingImpl(global)

  final def initializeCoreBTypes(): Unit = {
    coreBTypes.setBTypes(new CoreBTypes[this.type](this))
  }

  def recordPerRunCache[T <: collection.generic.Clearable](cache: T): T = perRunCaches.recordCache(cache)

  def compilerSettings: ScalaSettings = settings

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
   *
   * Note that the referenced class symbol may be an implementation class. For example when
   * compiling a mixed-in method that forwards to the static method in the implementation class,
   * the class descriptor of the receiver (the implementation class) is obtained by creating the
   * ClassBType.
   */
  final def classBTypeFromSymbol(classSym: Symbol): ClassBType = {
    assert(classSym != NoSymbol, "Cannot create ClassBType from NoSymbol")
    assert(classSym.isClass, s"Cannot create ClassBType from non-class symbol $classSym")
    assertClassNotArrayNotPrimitive(classSym)
    assert(!primitiveTypeMap.contains(classSym) || isCompilingPrimitive, s"Cannot create ClassBType for primitive class symbol $classSym")
    if (classSym == NothingClass) RT_NOTHING
    else if (classSym == NullClass) RT_NULL
    else {
      val internalName = classSym.javaBinaryName.toString
      classBTypeFromInternalName.getOrElse(internalName, {
        // The new ClassBType is added to the map in its constructor, before we set its info. This
        // allows initializing cyclic dependencies, see the comment on variable ClassBType._info.
        val res = ClassBType(internalName)
        if (completeSilentlyAndCheckErroneous(classSym)) {
          res.info = Left(NoClassBTypeInfoClassSymbolInfoFailedSI9111(classSym.fullName))
          res
        } else {
          setClassInfo(classSym, res)
        }
      })
    }
  }

  /**
   * Builds a [[MethodBType]] for a method symbol.
   */
  final def methodBTypeFromSymbol(methodSymbol: Symbol): MethodBType = {
    assert(methodSymbol.isMethod, s"not a method-symbol: $methodSymbol")
    val resultType: BType =
      if (methodSymbol.isClassConstructor || methodSymbol.isConstructor) UNIT
      else typeToBType(methodSymbol.tpe.resultType)
    MethodBType(methodSymbol.tpe.paramTypes map typeToBType, resultType)
  }

  /**
   * This method returns the BType for a type reference, for example a parameter type.
   *
   * If `t` references a class, typeToBType ensures that the class is not an implementation class.
   * See also comment on classBTypeFromSymbol, which is invoked for implementation classes.
   */
  final def typeToBType(t: Type): BType = {
    import definitions.ArrayClass

    /**
     * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
     * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
     */
    def primitiveOrClassToBType(sym: Symbol): BType = {
      assertClassNotArray(sym)
      assert(!sym.isImplClass, sym)
      primitiveTypeMap.getOrElse(sym, classBTypeFromSymbol(sym))
    }

    /**
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
     */
    def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
      assert(sym.isType && isCompilingArray, sym)
      ObjectReference
    }

    t.dealiasWiden match {
      case TypeRef(_, ArrayClass, List(arg))  => ArrayBType(typeToBType(arg)) // Array type such as Array[Int] (kept by erasure)
      case TypeRef(_, sym, _) if !sym.isClass => nonClassTypeRefToBType(sym)  // See comment on nonClassTypeRefToBType
      case TypeRef(_, sym, _)                 => primitiveOrClassToBType(sym) // Common reference to a type such as scala.Int or java.lang.String
      case ClassInfoType(_, _, sym)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes typeToBType(moduleClassSymbol.info)

      /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
       * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
       * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
       */
      case a @ AnnotatedType(_, t) =>
        debuglog(s"typeKind of annotated type $a")
        typeToBType(t)

      /* ExistentialType should (probably) be eliminated by erasure. We know they get here for
       * classOf constants:
       *   class C[T]
       *   class T { final val k = classOf[C[_]] }
       */
      case e @ ExistentialType(_, t) =>
        debuglog(s"typeKind of existential type $e")
        typeToBType(t)

      /* The cases below should probably never occur. They are kept for now to avoid introducing
       * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
       * test suite don't produce any warning.
       */

      case tp =>
        currentUnit.warning(tp.typeSymbol.pos,
          s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
            "If possible, please file a bug on issues.scala-lang.org.")

        tp match {
          case ThisType(ArrayClass)               => ObjectReference // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
          case ThisType(sym)                      => classBTypeFromSymbol(sym)
          case SingleType(_, sym)                 => primitiveOrClassToBType(sym)
          case ConstantType(_)                    => typeToBType(t.underlying)
          case RefinedType(parents, _)            => parents.map(typeToBType(_).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b).get)
        }
    }
  }

  def assertClassNotArray(sym: Symbol): Unit = {
    assert(sym.isClass, sym)
    assert(sym != definitions.ArrayClass || isCompilingArray, sym)
  }

  def assertClassNotArrayNotPrimitive(sym: Symbol): Unit = {
    assertClassNotArray(sym)
    assert(!primitiveTypeMap.contains(sym) || isCompilingPrimitive, sym)
  }

  private def setClassInfo(classSym: Symbol, classBType: ClassBType): ClassBType = {
    // Check for isImplClass: trait implementation classes have NoSymbol as superClass
    // Check for hasAnnotationFlag for SI-9393: the classfile / java source parsers add
    // scala.annotation.Annotation as superclass to java annotations. In reality, java
    // annotation classfiles have superclass Object (like any interface classfile).
    val superClassSym = if (classSym.isImplClass || classSym.hasJavaAnnotationFlag) ObjectClass else {
      val sc = classSym.superClass
      // SI-9393: Java annotation classes don't have the ABSTRACT/INTERFACE flag, so they appear
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
        ((superClassSym != NoSymbol) && !superClassSym.isInterface) || (isCompilingPrimitive && primitiveTypeMap.contains(classSym)),
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
     *
     * Nested classes that are also referenced in C will be added to the innerClassBufferASM during
     * code generation, but those duplicates will be eliminated when emitting the InnerClass
     * attribute.
     *
     * Why do we need to collect classes into innerClassBufferASM at all? To collect references to
     * nested classes, but NOT nested in C, that are used within C.
     */
    val nestedClassSymbols = {
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
        // If this is a top-level non-impl (*) class, the member classes of the companion object are
        // added as members of the class. For example:
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
        //
        // (*) We exclude impl classes: if the classfile for the impl class exists on the classpath,
        // a linkedClass symbol is found for which isTopLevelModule is true, so we end up searching
        // members of that weird impl-class-module-class-symbol. that search probably cannot return
        // any classes, but it's better to exclude it.
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

    /**
     * For nested java classes, the scala compiler creates both a class and a module (and therefore
     * a module class) symbol. For example, in `class A { class B {} }`, the nestedClassSymbols
     * for A contain both the class B and the module class B.
     * Here we get rid of the module class B, making sure that the class B is present.
     */
    val nestedClassSymbolsNoJavaModuleClasses = nestedClassSymbols.filter(s => {
      if (s.isJavaDefined && s.isModuleClass) {
        // We could also search in nestedClassSymbols for s.linkedClassOfClass, but sometimes that
        // returns NoSymbol, so it doesn't work.
        val nb = nestedClassSymbols.count(mc => mc.name == s.name && mc.owner == s.owner)
        assert(nb == 2, s"Java member module without member class: $s - $nestedClassSymbols")
        false
      } else true
    })

    val nestedClasses = nestedClassSymbolsNoJavaModuleClasses.map(classBTypeFromSymbol)

    val nestedInfo = buildNestedInfo(classSym)

    val inlineInfo = buildInlineInfo(classSym, classBType.internalName)

    classBType.info = Right(ClassInfo(superClass, interfaces, flags, nestedClasses, nestedInfo, inlineInfo))
    classBType
  }

  private def buildNestedInfo(innerClassSym: Symbol): Option[NestedInfo] = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")

    val isTopLevel = innerClassSym.rawowner.isPackageClass
    // impl classes are considered top-level, see comment in BTypes
    if (isTopLevel || considerAsTopLevelImplementationArtifact(innerClassSym)) None
    else if (innerClassSym.rawowner.isTerm) {
      // This case should never be reached: the lambdalift phase mutates the rawowner field of all
      // classes to be the enclosing class. SI-9392 shows an errant macro that leaves a reference
      // to a local class symbol that no longer exists, which is not updated by lambdalift.
      devWarning(innerClassSym.pos,
        s"""The class symbol $innerClassSym with the term symbol ${innerClassSym.rawowner} as `rawowner` reached the backend.
           |Most likely this indicates a stale reference to a non-existing class introduced by a macro, see SI-9392.""".stripMargin)
      None
    } else {
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
        if ((innerClassSym.isJavaDefined && innerClassSym.rawowner.isModuleClass) ||                      // (1)
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
        else Some(innerClassSym.rawname + innerClassSym.moduleSuffix) // moduleSuffix for module classes
      }

      Some(NestedInfo(enclosingClass, outerName, innerName, isStaticNestedClass))
    }
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
    def buildFromSymbol = buildInlineInfoFromClassSymbol(classSym, classBTypeFromSymbol(_).internalName, methodBTypeFromSymbol(_).descriptor)

    // phase travel required, see implementation of `compiles`. for nested classes, it checks if the
    // enclosingTopLevelClass is being compiled. after flatten, all classes are considered top-level,
    // so `compiles` would return `false`.
    if (exitingPickler(currentRun.compiles(classSym))) buildFromSymbol    // InlineInfo required for classes being compiled, we have to create the classfile attribute
    else if (!compilerSettings.YoptInlinerEnabled) BTypes.EmptyInlineInfo // For other classes, we need the InlineInfo only inf the inliner is enabled.
    else {
      // For classes not being compiled, the InlineInfo is read from the classfile attribute. This
      // fixes an issue with mixed-in methods: the mixin phase enters mixin methods only to class
      // symbols being compiled. For non-compiled classes, we could not build MethodInlineInfos
      // for those mixin members, which prevents inlining.
      byteCodeRepository.classNode(internalName) match {
        case Right(classNode) =>
          inlineInfoFromClassfile(classNode)
        case Left(missingClass) =>
          InlineInfo(None, false, Map.empty, Some(ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass)))
      }
    }
  }

  /**
   * For top-level objects without a companion class, the compilere generates a mirror class with
   * static forwarders (Java compat). There's no symbol for the mirror class, but we still need a
   * ClassBType (its info.nestedClasses will hold the InnerClass entries, see comment in BTypes).
   */
  def mirrorClassClassBType(moduleClassSym: Symbol): ClassBType = {
    assert(isTopLevelModuleClass(moduleClassSym), s"not a top-level module class: $moduleClassSym")
    val internalName = moduleClassSym.javaBinaryName.dropModule.toString
    classBTypeFromInternalName.getOrElse(internalName, {
      val c = ClassBType(internalName)
      // class info consistent with BCodeHelpers.genMirrorClass
      val nested = exitingPickler(memberClassesForInnerClassTable(moduleClassSym)) map classBTypeFromSymbol
      c.info = Right(ClassInfo(
        superClass = Some(ObjectReference),
        interfaces = Nil,
        flags = asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL,
        nestedClasses = nested,
        nestedInfo = None,
        InlineInfo(None, true, Map.empty, None))) // no InlineInfo needed, scala never invokes methods on the mirror class
      c
    })
  }

  /**
   * True for module classes of package level objects. The backend will generate a mirror class for
   * such objects.
   */
  final def isTopLevelModuleClass(sym: Symbol): Boolean = exitingPickler {
    // phase travel to pickler required for isNestedClass (looks at owner)
    val r = sym.isModuleClass && !sym.isNestedClass
    // The mixin phase adds the `lateMODULE` flag to trait implementation classes. Since the flag
    // is late, it should not be visible here inside the time travel. We check this.
    if (r) assert(!sym.isImplClass, s"isModuleClass should be false for impl class $sym")
    r
  }

  /**
   * True for module classes of modules that are top-level or owned only by objects. Module classes
   * for such objects will get a MODULE$ flag and a corresponding static initializer.
   */
  final def isStaticModuleClass(sym: Symbol): Boolean = {
    /* (1) Phase travel to to pickler is required to exclude implementation classes; they have the
     * lateMODULEs after mixin, so isModuleClass would be true.
     * (2) isStaticModuleClass is a source-level property. See comment on isOriginallyStaticOwner.
     */
    exitingPickler { // (1)
      sym.isModuleClass &&
      isOriginallyStaticOwner(sym.originalOwner) // (2)
    }
  }

  // legacy, to be removed when the @remote annotation gets removed
  final def isRemote(s: Symbol) = s hasAnnotation definitions.RemoteAttr
  final def hasPublicBitSet(flags: Int) = (flags & asm.Opcodes.ACC_PUBLIC) != 0

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
    // Top-level modules are marked ACC_FINAL in bytecode (even without the FINAL flag). Nested
    // objects don't get the flag to allow overriding (under -Yoverride-objects, SI-5676).
    //
    // For fields, only eager val fields can receive ACC_FINAL. vars or lazy vals can't:
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

    val finalFlag = (
      (((sym.rawflags & symtab.Flags.FINAL) != 0) || isTopLevelModuleClass(sym))
        && !sym.enclClass.isInterface
        && !sym.isClassConstructor
        && !sym.isMutable // lazy vals and vars both
      )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract if present.
    import asm.Opcodes._
    GenBCode.mkFlags(
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
      if (sym.hasFlag(symtab.Flags.SYNCHRONIZED)) ACC_SYNCHRONIZED else 0,
      if (sym.isDeprecated) asm.Opcodes.ACC_DEPRECATED else 0
    )
  }

  def javaFieldFlags(sym: Symbol) = {
    javaFlags(sym) | GenBCode.mkFlags(
      if (sym hasAnnotation TransientAttr) asm.Opcodes.ACC_TRANSIENT else 0,
      if (sym hasAnnotation VolatileAttr)  asm.Opcodes.ACC_VOLATILE  else 0,
      if (sym.isMutable) 0 else asm.Opcodes.ACC_FINAL
    )
  }
}
