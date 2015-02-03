/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import opt.ByteCodeRepository
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository.Source
import BTypes.InternalName

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

  val byteCodeRepository = new ByteCodeRepository(global.classPath, recordPerRunCache(collection.concurrent.TrieMap.empty[InternalName, (ClassNode, Source)]))

  final def initializeCoreBTypes(): Unit = {
    coreBTypes.setBTypes(new CoreBTypes[this.type](this))
  }

  def recordPerRunCache[T <: collection.generic.Clearable](cache: T): T = perRunCaches.recordCache(cache)

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
   * The ClassBType for a class symbol `sym`.
   */
  final def classBTypeFromSymbol(classSym: Symbol): ClassBType = {
    assert(classSym != NoSymbol, "Cannot create ClassBType from NoSymbol")
    assert(classSym.isClass, s"Cannot create ClassBType from non-class symbol $classSym")
    assert(
      (!primitiveTypeMap.contains(classSym) || isCompilingPrimitive) &&
      (classSym != NothingClass && classSym != NullClass),
      s"Cannot create ClassBType for special class symbol ${classSym.fullName}")

    val internalName = classSym.javaBinaryName.toString
    classBTypeFromInternalName.getOrElse(internalName, {
      // The new ClassBType is added to the map in its constructor, before we set its info. This
      // allows initializing cyclic dependencies, see the comment on variable ClassBType._info.
      setClassInfo(classSym, ClassBType(internalName))
    })
  }

  private def setClassInfo(classSym: Symbol, classBType: ClassBType): ClassBType = {
    val superClassSym = if (classSym.isImplClass) ObjectClass else classSym.superClass
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

    val flags = javaFlags(classSym)

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
          // classes, not local classes of the companion module (E in the exmaple) that were lifted by lambdalift.
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

    classBType.info = ClassInfo(superClass, interfaces, flags, nestedClasses, nestedInfo)
    classBType
  }

  private def buildNestedInfo(innerClassSym: Symbol): Option[NestedInfo] = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")

    val isTopLevel = innerClassSym.rawowner.isPackageClass
    // impl classes are considered top-level, see comment in BTypes
    if (isTopLevel || considerAsTopLevelImplementationArtifact(innerClassSym)) None
    else {
      // See comment in BTypes, when is a class marked static in the InnerClass table.
      val isStaticNestedClass = isOriginallyStaticOwner(innerClassSym.originalOwner)

      // After lambdalift (which is where we are), the rawowoner field contains the enclosing class.
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
      c.info = ClassInfo(
        superClass = Some(ObjectReference),
        interfaces = Nil,
        flags = asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL,
        nestedClasses = nested,
        nestedInfo = None
      )
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
  final def isRemote(s: Symbol) = (s hasAnnotation definitions.RemoteAttr)
  final def hasPublicBitSet(flags: Int) = ((flags & asm.Opcodes.ACC_PUBLIC) != 0)

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
      if (sym.hasEnumFlag) ACC_ENUM else 0,
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
