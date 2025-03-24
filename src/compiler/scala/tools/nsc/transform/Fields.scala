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
package transform

import scala.annotation._
import scala.reflect.internal.util.ListOfNil
import symtab.Flags._


/** Synthesize accessors, fields (and bitmaps) for (lazy) vals and modules.
  *
  * During Namers, a `ValDef` that is `lazy`, deferred and/or defined in a trait carries its getter's symbol.
  * The underlying field symbol does not exist until this phase.
  *
  * For `val`s defined in classes, we still emit a field immediately.
  * TODO: uniformly assign getter symbol to all `ValDef`s, stop using `accessed`.
  *
  * This phase synthesizes accessors, fields and bitmaps (for lazy or init-checked vals under -Xcheckinit)
  * in the first (closest in the subclassing lattice) subclass (not a trait) of a trait.
  *
  * For lazy vals and modules, we emit accessors that using double-checked locking (DCL) to balance thread safety
  * and performance. For both lazy vals and modules, the a compute method contains the DCL's slow path.
  *
  * Local lazy vals do not receive bitmaps, but use a Lazy*Holder that has the volatile init bit and the computed value.
  * See `mkLazyLocalDef`.
  *
  * Constructors will move the rhs to an assignment in the template body.
  * Those statements then move to the template into the constructor,
  * which means it will initialize the fields defined in this template (and execute the corresponding side effects).
  * We need to maintain the connection between getter and rhs until after specialization so that it can duplicate vals.
  *
  * A ModuleDef is desugared to a ClassDef, an accessor (which reuses the module's term symbol)
  * and a module var (unless the module is static and does not implement a member of a supertype, or we're in a trait).
  *
  * For subclasses of traits that define modules, a module var is mixed in, as well as the required module accessors.
  *
  * Phase ordering:
  *   - Runs after uncurry to deal with classes that implement SAM traits with ValDefs.
  *   - Runs before erasure (to get bridges), and thus before lambdalift/flatten, so that nested functions/definitions must be considered.
  *   - Lambdalift introduces new paramaccessors for captured vals, but runs too late in the pipeline, so
  *     mixins still synthesizes implementations for these accessors when a local trait that captures is subclassed.
  *
  *
  * In the future, would like to get closer to dotty, which lifts a val's RHS (a similar thing is done for template-level statements)
  * to a method `\$_initialize_\$1\$x` instead of a block, which is used in the constructor to initialize the val.
  * This makes for a nice unification of strict and lazy vals, in that the RHS is lifted to a method for both,
  * with the corresponding compute method called at the appropriate time.)
  *
  * This only reduces the required number of methods per field declaration in traits,
  * if we encode the name (and place in initialisation order) of the field
  * in the name of its initializing method, to allow separate compilation.
  * (The name mangling must include ordering, and thus complicate incremental compilation:
  * ideally, we'd avoid renumbering unchanged methods, but that would result in
  * different bytecode between clean recompiles and incremental ones).
  *
  * In the even longer term (Scala 3?), I agree with @DarkDimius that it would make sense
  * to hide the difference between strict and lazy vals. All vals are lazy,
  * but the memoization overhead is removed when we statically know they are forced during initialization.
  * We could still expose the low-level field semantics through `private[this] val`s.
  *
  * In any case, the current behavior of overriding vals is pretty surprising.
  * An overridden val's side-effect is still performed.
  * The only change due to overriding is that its value is never written to the field
  * (the overridden val's value is, of course, stored in the field in addition to its side-effect being performed).
  *
  * TODO: Java 9 support for vals defined in traits. They are currently emitted as final,
  *       but the write (putfield) to the val does not occur syntactically within the <init> method
  *       (it's done by the trait setter, which is called from the trait's mixin constructor,
  *       which is called from the subclass's constructor...)
  */
abstract class Fields extends InfoTransform with ast.TreeDSL with TypingTransformers with AccessorSynthesis {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "fields"

  protected def newTransformer(unit: CompilationUnit): AstTransformer = new FieldsTransformer(unit)
  override def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isJavaDefined || sym.isPackageClass || !sym.isClass) tp
    else synthFieldsAndAccessors(tp)

  // TODO: drop PRESUPER support when we implement trait parameters in 2.13
  private def excludedAccessorOrFieldByFlags(statSym: Symbol): Boolean = statSym hasFlag PRESUPER

  // used for internal communication between info and tree transform of this phase -- not pickled, not in initialflags
  // TODO: reuse MIXEDIN for NEEDS_TREES?
  override def phaseNewFlags: Long = NEEDS_TREES | OVERRIDDEN_TRAIT_SETTER

  // informs the tree traversal of the shape of the tree to emit
  // (it's an *overridden* trait setter)
  private final val OVERRIDDEN_TRAIT_SETTER = TRANS_FLAG

  final val TRAIT_SETTER_FLAGS = NEEDS_TREES | DEFERRED | ProtectedLocal

  private def accessorImplementedInSubclass(accessor: Symbol) =
    (accessor hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS) && (accessor hasFlag (ACCESSOR | MODULE))

  @inline final def notDeferredOrSynthImpl(sym: Symbol): Boolean = !(sym hasFlag DEFERRED) || (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)

  private def synthesizeImplInSubclasses(accessor: Symbol): Unit =
    accessor setFlag SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setClonedTraitSetterFlags(clazz: Symbol, correspondingGetter: Symbol, cloneInSubclass: Symbol): Unit = {
    val overridden = isOverriddenAccessor(correspondingGetter, clazz)
    if (overridden) cloneInSubclass setFlag OVERRIDDEN_TRAIT_SETTER
    else if (correspondingGetter.isEffectivelyFinal) cloneInSubclass setFlag FINAL
  }

  // TODO: add MIXEDIN (see e.g., `accessed` on `Symbol`)
  private def setMixedinAccessorFlags(@unused orig: Symbol, cloneInSubclass: Symbol): Unit =
    cloneInSubclass setFlag OVERRIDE | NEEDS_TREES resetFlag DEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setFieldFlags(accessor: Symbol, fieldInSubclass: TermSymbol): Unit = {
    // Since initialization is performed (lexically) outside of the constructor (in the trait setter),
    // we have to make the field mutable starting with classfile format 53
    // (it was never allowed, but the verifier enforces this now).
    fieldInSubclass.setFlag(NEEDS_TREES | PrivateLocal | MUTABLE | accessor.getFlag(LAZY | DEFAULTINIT))
    if (accessor.hasFlag(STABLE)) {
      // If the field is for an immutable val, make sure it's safely published
      val isInStaticModule = fieldInSubclass.owner.isModuleClass && fieldInSubclass.owner.sourceModule.isStaticModule
      if (!isInStaticModule) // the <clinit> lock is enough.
        fieldInSubclass.owner.primaryConstructor.updateAttachment(ConstructorNeedsFence)
    }
  }

  def checkAndClearOverriddenTraitSetter(setter: Symbol) = checkAndClear(OVERRIDDEN_TRAIT_SETTER)(setter)
  def checkAndClearNeedsTrees(setter: Symbol) = checkAndClear(NEEDS_TREES)(setter)
  def checkAndClear(flag: Long)(sym: Symbol) =
    if (sym.hasFlag(flag)) {
      sym resetFlag flag
      true
    } else false


  private def isOverriddenAccessor(member: Symbol, site: Symbol): Boolean = {
    val pre = site.thisType
    @tailrec def loop(bcs: List[Symbol]): Boolean = {
      //      println(s"checking ${bcs.head} for member overriding $member (of ${member.owner})")
      bcs.nonEmpty && bcs.head != member.owner && (matchingAccessor(pre, member, bcs.head) != NoSymbol || loop(bcs.tail))
    }

    member.exists && loop(site.info.baseClasses)
  }


  def matchingAccessor(pre: Type, member: Symbol, clazz: Symbol) = {
    val res = member.matchingSymbol(clazz, pre) filter (sym => (sym hasFlag ACCESSOR) && notDeferredOrSynthImpl(sym))
    //    if (res != NoSymbol) println(s"matching accessor for $member in $clazz = $res (under $pre)")
    //    else println(s"no matching accessor for $member in $clazz (under $pre) among ${clazz.info.decls}")
    res
  }


  class FieldMemoization(accessorOrField: Symbol, site: Symbol) {
    val tp           = fieldTypeOfAccessorIn(accessorOrField, site.thisType)
    // We can only omit strict vals of ConstantType. Lazy vals do not receive constant types (anymore).
    // (See note at widenIfNecessary -- for example, the REPL breaks when we omit constant lazy vals)
    // Note that a strict unit-typed val does receive a field, because we cannot omit the write to the field
    // (well, we could emit it for non-@volatile ones, if I understand the memory model correctly,
    //  but that seems pretty edge-casey)
    val constantTyped = tp.isInstanceOf[FoldableConstantType]
  }

  private def fieldTypeForGetterIn(getter: Symbol, pre: Type): Type = getter.info.finalResultType.asSeenFrom(pre, getter.owner)
  private def fieldTypeForSetterIn(setter: Symbol, pre: Type): Type = setter.info.paramTypes.head.asSeenFrom(pre, setter.owner)

  // TODO: is there a more elegant way?
  def fieldTypeOfAccessorIn(accessor: Symbol, pre: Type) =
    if (accessor.isSetter) fieldTypeForSetterIn(accessor, pre)
    else fieldTypeForGetterIn(accessor, pre)


  // Constant/unit typed vals are not memoized (their value is so cheap it doesn't make sense to store it in a field)
  // for a unit-typed getter, we perform the effect at the appropriate time (constructor for eager ones, lzyCompute for lazy),
  // and have the getter just return Unit (who does that!?)
  // NOTE: this only considers type, filter on flags first!
  def fieldMemoizationIn(accessorOrField: Symbol, site: Symbol) = new FieldMemoization(accessorOrField, site)

  // drop field-targeting annotations from getters (done during erasure because we first need to create the field symbol)
  // (in traits, getters must also hold annotations that target the underlying field,
  //  because the latter won't be created until the trait is mixed into a class)
  // TODO do bean getters need special treatment to suppress field-targeting annotations in traits?
  def dropFieldAnnotationsFromGetter(sym: Symbol): Unit =
    sym.setAnnotations(sym.annotations.filter(AnnotationInfo.mkFilter(GetterTargetClass, defaultRetention = false)))

  def symbolAnnotationsTargetFieldAndGetter(sym: Symbol): Boolean = sym.isGetter && (sym.isLazy || sym.owner.isTrait)

  // A trait val/var or a lazy val does not receive an underlying field symbol until this phase.
  // Since annotations need a carrier symbol from the beginning, both field- and getter-targeting annotations
  // are kept on the getter symbol for these until they are dropped by dropFieldAnnotationsFromGetter
  def getterTreeAnnotationsTargetFieldAndGetter(owner: Symbol, mods: Modifiers) = mods.isLazy || owner.isTrait

  // Propagate field-targeting annotations from getter to field.
  // By the way, we must keep them around long enough to see them here (now that we have created the field),
  // which is why dropFieldAnnotationsFromGetter is not called until erasure.
  private def propagateFieldAnnotations(getter: Symbol, field: TermSymbol): Unit =
    field setAnnotations (getter.annotations filter AnnotationInfo.mkFilter(FieldTargetClass, defaultRetention = true))


  // can't use the referenced field since it already tracks the module's moduleClass
  private[this] val moduleOrLazyVarOf = perRunCaches.newMap[Symbol, Symbol]()

  // TODO: can we drop FINAL? In any case, since these variables are MUTABLE, they cannot and will
  // not be emitted as ACC_FINAL. They are FINAL in the Scala sense, though: cannot be overridden.
  private final val ModuleOrLazyFieldFlags = FINAL | PrivateLocal | SYNTHETIC | NEEDS_TREES

  private def moduleInit(module: Symbol, moduleVar: Symbol) = {
//    println(s"moduleInit for $module in ${module.ownerChain} --> ${moduleVarOf.get(module)}")
    def moduleVarRef = gen.mkAttributedRef(moduleVar)

    // for local modules, we synchronize on the owner of the method that owns the module
    val monitorHolder = This(moduleVar.owner.enclClass)
    def needsInit = Apply(Select(moduleVarRef, Object_eq), List(CODE.NULL))
    val init = Assign(moduleVarRef, gen.newModule(module, moduleVar.info))

    /* double-checked locking following https://shipilev.net/blog/2014/safe-public-construction/#_safe_publication
     *
     * public class SafeDCLFactory {
     *   private volatile Singleton instance;
     *
     *   public Singleton get() {
     *     if (instance == null) {  // check 1
     *       synchronized(this) {
     *         if (instance == null) { // check 2
     *           instance = new Singleton();
     *         }
     *       }
     *     }
     *     return instance;
     *   }
     * }
     *
     * TODO: optimize using local variable?
     */
    val computeName = nme.newLazyValSlowComputeName(module.name)
    val computeMethod = DefDef(NoMods, computeName, Nil, ListOfNil, TypeTree(UnitTpe), gen.mkSynchronized(monitorHolder)(If(needsInit, init, EmptyTree)))
    Block(computeMethod :: If(needsInit, Apply(Ident(computeName), Nil), EmptyTree) :: Nil,
      gen.mkCast(moduleVarRef, module.info.resultType))
  }

  // NoSymbol for lazy accessor sym with unit result type
  def lazyVarOf(sym: Symbol) = moduleOrLazyVarOf.getOrElse(sym, NoSymbol)

  private def newLazyVarMember(clazz: Symbol, member: Symbol, tp: Type): TermSymbol = {
    val flags = LAZY | (member.flags & FieldFlags) | ModuleOrLazyFieldFlags
    val name  = member.name.toTermName.append(reflect.NameTransformer.LOCAL_SUFFIX_STRING)

    // Set the MUTABLE flag because the field cannot be ACC_FINAL since we write to it outside of a constructor.
    val sym = clazz.newVariable(name, member.pos.focus, flags) setInfo tp

    moduleOrLazyVarOf(member) = sym
    sym
  }


  private object synthFieldsAndAccessors extends TypeMap {
    private def newTraitSetter(getter: Symbol, clazz: Symbol) = {
      // Add setter for an immutable, memoizing getter
      // (can't emit during namers because we don't yet know whether it's going to be memoized or not)
      val setterFlags = (getter.flags & ~(STABLE | PrivateLocal | OVERRIDE | IMPLICIT | FINAL)) | MUTABLE | ACCESSOR | TRAIT_SETTER_FLAGS
      val setterName  = nme.expandedSetterName(getter.name.setterName, clazz)
      val setter      = clazz.newMethod(setterName, getter.pos.focus, setterFlags)
      val fieldTp     = fieldTypeForGetterIn(getter, clazz.thisType)
      // println(s"newTraitSetter in $clazz for $getter = $setterName : $fieldTp")

      getter.asTerm.referenced = setter

      setter setInfo MethodType(List(setter.newSyntheticValueParam(fieldTp)), UnitTpe)
      setter
    }

    private def newModuleAccessor(module: Symbol, site: Symbol, moduleVar: Symbol) = {
      val accessor = site.newMethod(module.name.toTermName, site.pos, STABLE | MODULE | NEEDS_TREES)

      moduleOrLazyVarOf(accessor) = moduleVar

      // we're in the same prefix as module, so no need for site.thisType.memberType(module)
      accessor setInfo MethodType(Nil, moduleVar.info)
      accessor.setModuleClass(module.moduleClass)

      if (module.isPrivate) accessor.expandName(module.owner)

      accessor
    }

    // needed for the following scenario (T could be trait or class)
    // trait T { def f: Object }; object O extends T { object f }. Need to generate method f in O.
    // marking it as an ACCESSOR so that it will get to `getterBody` when synthesizing trees below
    // it should not be considered a MODULE
    def newMatchingModuleAccessor(clazz: Symbol, module: Symbol): MethodSymbol = {
      val acc = clazz.newMethod(module.name.toTermName, module.pos, (module.flags & ~MODULE) | STABLE | NEEDS_TREES | ACCESSOR)
      acc.referenced = module
      acc setInfo MethodType(Nil, module.moduleClass.tpe)
    }


    private def newSuperLazy(lazyCallingSuper: Symbol, site: Type, lazyVar: Symbol) = {
      lazyCallingSuper.asTerm.referenced = lazyVar

      val tp = resultTypeMemberOfDeconst(site, lazyCallingSuper)

      lazyVar setInfo tp
      lazyCallingSuper setInfo MethodType(Nil, tp)
    }

    private def classNeedsInfoTransform(cls: Symbol): Boolean = {
      !(cls.isPackageClass || cls.isJavaDefined) && (currentRun.compiles(cls) || refChecks.isSeparatelyCompiledScalaSuperclass(cls))
    }

    def apply(tp0: Type): Type = tp0 match {
      // TODO: make less destructive (name changes, decl additions, flag setting --
      // none of this is actually undone when travelling back in time using atPhase)
      case tp@ClassInfoType(parents, decls, clazz) if clazz.isTrait =>
        // setters for trait vars or module accessor
        val newDecls = collection.mutable.ListBuffer[Symbol]()
        val origDecls = decls.toList

        // strict, memoized accessors will receive an implementation in first real class to extend this trait
        origDecls.foreach { member =>
          if (member hasFlag ACCESSOR) {
            val fieldMemoization = fieldMemoizationIn(member, clazz)
            // check flags before calling makeNotPrivate
            val accessorUnderConsideration = !(member hasFlag DEFERRED)

            // destructively mangle accessor's name (which may cause rehashing of decls), also sets flags
            // this accessor has to be implemented in a subclass -- can't be private
            if ((member hasFlag PRIVATE) && !fieldMemoization.constantTyped) member makeNotPrivate clazz
            // Since we need to refer to `member` using a super call in a subclass, we must ensure that access is allowed.
            // If `member` has an access boundary, make sure the `PROTECTED` flag is set,
            // to widen from `private[foo]` to `protected[foo]`
            // (note that `member.hasAccessBoundary` implies `!member.hasFlag(PRIVATE)`, so we don't have to `resetFlag PRIVATE`)
            else if (member.isLazy && member.hasAccessBoundary) member setFlag PROTECTED

            // This must remain in synch with publicizeTraitMethod in Mixins, so that the
            // synthesized member in a subclass and the trait member remain in synch regarding access.
            // Otherwise, the member will not be seen as overriding the trait member, and `addForwarders`'s call to
            // `membersBasedOnFlags` would see the deferred member in the trait, instead of the concrete (desired) one in the class
            // not doing: if (member hasFlag PROTECTED) member setFlag notPROTECTED

            // must not reset LOCAL, as we must maintain protected[this]ness to allow that variance hole
            // (not sure why this only problem only arose when we started setting the notPROTECTED flag)

            // derive trait setter after calling makeNotPrivate (so that names are mangled consistently)
            if (accessorUnderConsideration && !fieldMemoization.constantTyped) {
              synthesizeImplInSubclasses(member)

              if ((member hasFlag STABLE) && !(member hasFlag LAZY))
                newDecls += newTraitSetter(member, clazz)
            }
          } else if (member.hasFlag(MODULE)) {
            nonStaticModuleToMethod(member)

            member setFlag NEEDS_TREES
            synthesizeImplInSubclasses(member)
          }
        }

        if (newDecls.nonEmpty) {
          val allDecls = newScope
          origDecls.foreach(allDecls.enter(_))
          newDecls .foreach(allDecls.enter(_))
          ClassInfoType(parents, allDecls, clazz)
        } else tp


      case tp@ClassInfoType(_, _, clazz) if !classNeedsInfoTransform(clazz) => tp

      // mix in fields & accessors for all mixed in traits
      case tp@ClassInfoType(parents, oldDecls, clazz) =>

        val site = clazz.thisType

        // setter conflicts cannot arise independently from a getter conflict, since a setter without a getter does not a val definition make
        def getterConflictsExistingVal(getter: Symbol): Boolean =
          getter.isGetter && {
            val existingGetter = oldDecls.lookup(getter.name)
            (existingGetter ne NoSymbol) &&
              ((site memberInfo existingGetter) matches (site memberInfo getter))
          }

        def newModuleVarMember(module: Symbol): TermSymbol = {
          val moduleVar =
            (clazz.newVariable(nme.moduleVarName(module.name.toTermName), module.pos.focus, MODULEVAR | ModuleOrLazyFieldFlags)
             setInfo resultTypeMemberOfDeconst(site, module)
             addAnnotation VolatileAttr)

          if (module.hasAnnotation(TransientAttr)) moduleVar.addAnnotation(TransientAttr)

          moduleOrLazyVarOf(module) = moduleVar

          moduleVar
        }

        def newLazyVarMember(member: Symbol): TermSymbol =
          Fields.this.newLazyVarMember(clazz, member, resultTypeMemberOfDeconst(site, member))

        // a module does not need treatment here if it's static, unless it has a matching member in a superclass
        // a non-static method needs a module var
        val modulesAndLazyValsNeedingExpansion =
          oldDecls.toList.filter(m => (m.isModule && (!m.isStatic || m.isOverridingSymbol)) || m.isLazy)

        val accessorSymbolSynth = checkedAccessorSymbolSynth(tp.typeSymbol)

        // expand module def in class/object (if they need it -- see modulesNeedingExpansion above)
        val expandedModulesAndLazyVals =
          modulesAndLazyValsNeedingExpansion flatMap { member =>
            if (member.isLazy) {
              val lazyVar = newLazyVarMember(member)
              propagateFieldAnnotations(member, lazyVar)
              List(lazyVar, accessorSymbolSynth.newSlowPathSymbol(member))
            }
            // expanding module def (top-level or nested in static module)
            else List(if (member.isStatic) { // implies m.isOverridingSymbol as per above filter
              // Need a module accessor, to implement/override a matching member in a superclass.
              // Never a need for a module var if the module is static.
              newMatchingModuleAccessor(clazz, member)
            } else {
              nonStaticModuleToMethod(member)
              // must reuse symbol instead of creating an accessor
              member setFlag NEEDS_TREES
              newModuleVarMember(member)
            })
          }

//        println(s"expanded modules for $clazz: $expandedModules")

        // afterOwnPhase, so traits receive trait setters for vals (needs to be at finest grain to avoid looping)
        val synthInSubclass: List[Symbol] =
          clazz.mixinClasses.flatMap(mixin => afterOwnPhase(mixin.info).decls.toList.filter(accessorImplementedInSubclass))

        // mixin field accessors --
        // invariant: (accessorsMaybeNeedingImpl, mixedInAccessorAndFields).zipped.forall(case (acc, clone :: _) => `clone` is clone of `acc` case _ => true)
        val mixedInAccessorAndFields: List[List[Symbol]] = synthInSubclass.map{ member =>
          def cloneAccessor() = {
            val clonedAccessor = (member cloneSymbol clazz) setPos clazz.pos
            setMixedinAccessorFlags(member, clonedAccessor)

            // note: check original member when deciding how to triage annotations, then act on the cloned accessor
            if (symbolAnnotationsTargetFieldAndGetter(member))  // this simplifies to member.isGetter, but the full formulation really ties the triage together
              dropFieldAnnotationsFromGetter(clonedAccessor)

            // if we don't cloneInfo, method argument symbols are shared between trait and subclasses --> lambdalift proxy crash
            // TODO: use derive symbol variant?
//            println(s"cloning accessor $member to $clazz")
            // start at uncurry so that we preserve that part of the history where an accessor has a NullaryMethodType
            enteringUncurry { clonedAccessor setInfo ((clazz.thisType memberType member) cloneInfo clonedAccessor) }
            clonedAccessor
          }

          // when considering whether to mix in the trait setter, forget about conflicts -- they are reported for the getter
          // a trait setter for an overridden val will receive a unit body in the tree transform
          if (nme.isTraitSetterName(member.name)) {
            val getter = member.getterIn(member.owner)
            val clone = cloneAccessor()

            setClonedTraitSetterFlags(clazz, getter, clone)
            // println(s"mixed in trait setter ${clone.defString}")

            List(clone)
          }
          // don't cause conflicts, skip overridden accessors contributed by supertraits (only act on the last overriding one)
          // see pos/trait_fields_dependent_conflict.scala and neg/t1960.scala
          else if (getterConflictsExistingVal(member) || isOverriddenAccessor(member, clazz)) Nil
          else if (member hasFlag MODULE) {
            val moduleVar = newModuleVarMember(member)
            List(moduleVar, newModuleAccessor(member, clazz, moduleVar))
          }
          else if (member hasFlag LAZY) {
            val mixedinLazy = cloneAccessor()
            val lazyVar = newLazyVarMember(mixedinLazy) // link lazy var member to the mixedin lazy accessor

            // propagate from original member. since mixed in one has only retained the annotations targeting the getter
            propagateFieldAnnotations(member, lazyVar)

            // println(s"mixing in lazy var: $lazyVar for $member")
            List(lazyVar, accessorSymbolSynth.newSlowPathSymbol(mixedinLazy), newSuperLazy(mixedinLazy, site, lazyVar))
          }
          else if (member.isGetter && !fieldMemoizationIn(member, clazz).constantTyped) {
            // add field if needed
            val field = clazz.newValue(member.localName, member.pos) setInfo fieldTypeForGetterIn(member, clazz.thisType)

            setFieldFlags(member, field)

            propagateFieldAnnotations(member, field)

            List(cloneAccessor(), field)
          } else List(cloneAccessor()) // no field needed (constant-typed getter has constant as its RHS)
        }

        //        println(s"mixedInAccessorAndFields for $clazz: $mixedInAccessorAndFields")

        // omit fields that are not memoized, retain all other members
        def omittableField(sym: Symbol) = sym.isValue && !sym.isMethod && fieldMemoizationIn(sym, clazz).constantTyped

        val newDecls =
          // under -Xcheckinit we generate all kinds of bitmaps, even when there are no lazy vals
          if (expandedModulesAndLazyVals.isEmpty && mixedInAccessorAndFields.isEmpty && !settings.checkInit.value)
            oldDecls.filterNot(omittableField)
          else {
            // must not alter `decls` directly
            val newDecls = newScope
            val enter    = newDecls enter (_: Symbol)
            val enterAll = (_: List[Symbol]) foreach enter

            expandedModulesAndLazyVals foreach enter
            oldDecls foreach { d => if (!omittableField(d)) enter(d) }
            mixedInAccessorAndFields foreach enterAll

            // both oldDecls and mixedInAccessorAndFields (a list of lists) contribute
            val bitmapSyms = accessorSymbolSynth.computeBitmapInfos(newDecls.filter(sym => sym.isValue && !sym.isMethod).toList)

            bitmapSyms foreach enter

            newDecls
          }

        //        println(s"new decls for $clazz: $expandedModules ++ $mixedInAccessorAndFields")

        if (newDecls eq oldDecls) tp
        else ClassInfoType(parents, newDecls, clazz)

      case tp => mapOver(tp)
    }
  }


  // Deconst the result type of the getter (not relevant for modules, but we'll just be consistent),
  // since we're after typers -- it's between unnecessary and wrong to keep constant types for result types of methods
  // constant folding has already happened during typer. We can keep literal types around until erasure,
  // but we shouldn't assume expressions of these types to be pure/constant foldable. (See pos/t10768.scala)
  private def resultTypeMemberOfDeconst(site: Type, acc: Symbol) = site.memberType(acc).resultType.deconst

  // done by uncurry's info transformer
  // instead of forcing every member's info to run said transformer, duplicate the flag update logic...
  def nonStaticModuleToMethod(module: Symbol): Unit =
    if (!module.isStatic) module setFlag METHOD | STABLE

  // scala/scala-dev#219, scala/scala-dev#268
  // Cast to avoid spurious mismatch in paths containing trait vals that have
  // not been rebound to accessors in the subclass we're in now.
  // For example, for a lazy val mixed into a class, the lazy var's info
  // will not refer to symbols created during our info transformer,
  // so if its type depends on a val that is now implemented after the info transformer,
  // we'll get a mismatch when assigning `rhs` to `lazyVarOf(getter)`.
  // TODO: could we rebind more aggressively? consider overriding in type equality?
  def castHack(tree: Tree, pt: Type) = gen.mkAsInstanceOf(tree, pt)

  class FieldsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with CheckedAccessorTreeSynthesis {
    protected def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    def mkTypedUnit(pos: Position) = typedPos(pos)(CODE.UNIT)
    // TODO: clean up. this method is not used
    def deriveUnitDef(stat: Tree)  = deriveDefDef(stat)(_ => mkTypedUnit(stat.pos))

    def mkAccessor(accessor: Symbol)(body: Tree) = typedPos(accessor.pos)(DefDef(accessor, body)).asInstanceOf[DefDef]

    // this makes trees for mixed in fields, as well as for bitmap fields (their RHS will be EmptyTree because they are initialized implicitly)
    // if we decide to explicitly initialize, use this RHS: if (symbol.info.typeSymbol.asClass == BooleanClass) FALSE else ZERO)
    // could detect it's a bitmap field with something like `sym.name.startsWith(nme.BITMAP_PREFIX)` (or perhaps something more robust...)
    def mkTypedValDef(sym: Symbol, rhs: Tree = EmptyTree) = typedPos(sym.pos)(ValDef(sym, rhs)).asInstanceOf[ValDef]

    /**
      * Desugar a local `lazy val x: Int = rhs`
      * or a local `object x { ...}` (the rhs will be instantiating the module's class) into:
      *
      * {{{
      * val x\$lzy = new scala.runtime.LazyInt()
      * def x\$lzycompute(): Int =
      *   x\$lzy.synchronized {
      *     if (x\$lzy.initialized()) x\$lzy.value()
      *     else x\$lzy.initialize(rhs) // for a Unit-typed lazy val, this becomes `{ rhs ; x\$lzy.initialize() }` to avoid passing around BoxedUnit
      *  }
      * def x(): Int = if (x\$lzy.initialized()) x\$lzy.value() else x\$lzycompute()
      * }}}
      *
      * The expansion is the same for local lazy vals and local objects,
      * except for the suffix of the underlying val's name (\$lzy or \$module)
      */
    private def mkLazyLocalDef(lazySym: Symbol, rhs: Tree): Tree = {
      import CODE._
      import scala.reflect.{NameTransformer => nx}
      val owner = lazySym.owner

      val lazyValType = lazySym.info.resultType.deconst
      val refClass    = lazyHolders.getOrElse(lazyValType.typeSymbol, LazyRefClass)
      val isUnit      = refClass == LazyUnitClass
      val refTpe      = if (refClass != LazyRefClass) refClass.tpe else appliedType(refClass.typeConstructor, List(lazyValType))

      val lazyName  = lazySym.name.toTermName
      val pos       = lazySym.pos.focus

      val localLazyName = lazyName append (if (lazySym.isModule) nx.MODULE_VAR_SUFFIX_STRING else nx.LAZY_LOCAL_SUFFIX_STRING)

      // The lazy holder val need not be mutable, as we write to its field.
      // In fact, it MUST not be mutable to avoid capturing it as an ObjectRef in lambdalift
      // Must be marked LAZY to allow forward references, as in `def test2 { println(s.length) ; lazy val s = "abc" }
      val holderSym = owner.newValue(localLazyName, pos, LAZY | ARTIFACT) setInfo refTpe

      val initializedSym = refTpe.member(nme.initialized)
      val initializeSym  = refTpe.member(nme.initialize)

      // LazyUnit does not have a `value` member
      val valueSym = if (isUnit) NoSymbol else refTpe.member(nme.value)

      def refineLiteral(tree: Tree): Tree =
        lazyValType match {
          case _: ConstantType => gen.mkAsInstanceOf(tree, lazyValType)
          case _ => tree
        }

      def initialized = Apply(Select(Ident(holderSym), initializedSym), Nil)
      def initialize  = Select(Ident(holderSym), initializeSym)
      def getValue    = if (isUnit) UNIT else refineLiteral(Apply(Select(Ident(holderSym), valueSym), Nil))

      val computerSym =
        owner.newMethod(lazyName append nme.LAZY_SLOW_SUFFIX, pos, ARTIFACT | PRIVATE) setInfo MethodType(Nil, lazyValType)

      val rhsAtComputer = rhs.changeOwner(lazySym, computerSym)

      val computer = mkAccessor(computerSym)(gen.mkSynchronized(Ident(holderSym))(
        If(initialized, getValue,
          if (isUnit) Block(rhsAtComputer :: Nil, Apply(initialize, Nil))
          else refineLiteral(Apply(initialize, rhsAtComputer :: Nil)))))

      val accessor = mkAccessor(lazySym)(
        refineLiteral(
          If(initialized, getValue,
            Apply(Ident(computerSym), Nil))))

      // do last!
      // remove STABLE: prevent replacing accessor call of type Unit by BoxedUnit.UNIT in erasure
      // remove ACCESSOR: prevent constructors from eliminating the method body if the lazy val is
      // lifted into a trait (TODO: not sure about the details here)
      lazySym.resetFlag(STABLE | ACCESSOR)

      Thicket(mkTypedValDef(holderSym, New(refTpe)) :: computer :: accessor :: Nil)
    }

    // synth trees for accessors/fields and trait setters when they are mixed into a class
    def fieldsAndAccessors(clazz: Symbol): List[Tree] = {

      // Could be NoSymbol, which denotes an error, but it's refchecks' job to report it (this fallback is for robustness).
      // This is the result of overriding a val with a def, so that no field is found in the subclass.
      def fieldAccess(accessor: Symbol): Symbol =
        afterOwnPhase { clazz.info.decl(accessor.localName) }

      def getterBody(getter: Symbol): Tree =
        // accessor created by newMatchingModuleAccessor for a static module that does need an accessor
        // (because there's a matching member in a super class)
        if (getter.asTerm.referenced.isModule)
          mkAccessor(getter)(castHack(Select(This(clazz), getter.asTerm.referenced), getter.info.resultType))
        else {
          val fieldMemoization = fieldMemoizationIn(getter, clazz)
          // TODO: drop getter for constant? (when we no longer care about producing identical bytecode?)
          if (fieldMemoization.constantTyped) mkAccessor(getter)(gen.mkAttributedQualifier(fieldMemoization.tp))
          else fieldAccess(getter) match {
            case NoSymbol => EmptyTree
            case fieldSel => mkAccessor(getter)(castHack(Select(This(clazz), fieldSel), getter.info.resultType))
          }
        }

      //      println(s"accessorsAndFieldsNeedingTrees for $templateSym: $accessorsAndFieldsNeedingTrees")
      def setterBody(setter: Symbol): Tree =
        // trait setter in trait
        if (clazz.isTrait) mkAccessor(setter)(EmptyTree)
        // trait setter for overridden val in class
        else if (checkAndClearOverriddenTraitSetter(setter)) mkAccessor(setter)(mkTypedUnit(setter.pos))
        // trait val/var setter mixed into class
        else fieldAccess(setter) match {
          case NoSymbol => EmptyTree
          case fieldSel => afterOwnPhase { // the assign only type checks after our phase (assignment to val)
            mkAccessor(setter)(Assign(Select(This(clazz), fieldSel), castHack(Ident(setter.firstParam), fieldSel.info)))
          }
        }

      def moduleAccessorBody(module: Symbol): Tree =
        // added during synthFieldsAndAccessors using newModuleAccessor
        // a module defined in a trait by definition can't be static (it's a member of the trait and thus gets a new instance for every outer instance)
        if (clazz.isTrait) mkAccessor(module)(EmptyTree)
        // symbol created by newModuleAccessor for a (non-trait) class
        else {
          mkAccessor(module)(moduleInit(module, moduleOrLazyVarOf(module)))
        }

      val synthAccessorInClass = new SynthLazyAccessorsIn(clazz)
      def superLazy(getter: Symbol): Tree = {
        assert(!clazz.isTrait, clazz)
        // this contortion was the only way I can get the super select to be type checked correctly..
        // TODO: why does SelectSuper not work?
        val selectSuper = Select(Super(This(clazz), tpnme.EMPTY), getter.name)

        val lazyVar = lazyVarOf(getter)
        val rhs = castHack(Apply(selectSuper, Nil), lazyVar.info)

        synthAccessorInClass.expandLazyClassMember(lazyVar, getter, rhs)
      }

      afterOwnPhase(clazz.info.decls).toList.filter(checkAndClearNeedsTrees).map {
        case module if module hasAllFlags (MODULE | METHOD) => moduleAccessorBody(module)
        case getter if getter hasAllFlags (LAZY | METHOD)   => superLazy(getter)
        case setter if setter.isSetter                      => setterBody(setter)
        case getter if getter.hasFlag(ACCESSOR)             => getterBody(getter)
        case field  if !(field hasFlag METHOD)              => mkTypedValDef(field) // vals/vars and module vars (cannot have flags PACKAGE | JAVA since those never receive NEEDS_TREES)
        case _ => EmptyTree
      }.filterNot(_ == EmptyTree) // there will likely be many EmptyTrees, but perhaps no thicket blocks that need expanding
    }

    def rhsAtOwner(stat: ValOrDefDef, newOwner: Symbol): Tree =
      atOwner(newOwner)(super.transform(stat.rhs.changeOwner(stat.symbol, newOwner)))

    override def transform(stat: Tree): Tree = {
      val currOwner = currentOwner // often a class, but not necessarily
      val statSym = stat.symbol

      /*
        For traits, the getter has the val's RHS, which is already constant-folded. There is no valdef.
        For classes, we still have the classic scheme of private[this] valdef + getter & setter that read/assign to the field.

        There are two axes: (1) is there a side-effect to the val (2) does the val need storage?
        For a ConstantType, both answers are "no". (For a unit-typed field, there's a side-effect, but no storage needed.)

        All others (getter for trait field, valdef for class field) have their rhs moved to an initialization statement.
        Trait accessors for stored fields are made abstract (there can be no field in a trait).
        (In some future version, accessors for non-stored, but effectful fields,
         would receive a constant rhs, as the effect is performed by the initialization statement.
         We could do this for unit-typed fields, but have chosen not to for backwards compatibility.)
       */
      stat match {
        // TODO: consolidate with ValDef case
        // TODO: defer replacing ConstantTyped tree by the corresponding constant until erasure
        // (until then, trees should not be constant-folded -- only their type tracks the resulting constant)
        // also remove ACCESSOR flag since there won't be an underlying field to access?
        case DefDef(_, _, _, _, _, rhs) if (statSym hasFlag ACCESSOR)
                                           && (rhs ne EmptyTree) && !excludedAccessorOrFieldByFlags(statSym)
                                           && !currOwner.isTrait // we've already done this for traits.. the asymmetry will be solved by the above todo
                                           && fieldMemoizationIn(statSym, currOwner).constantTyped =>
          deriveDefDef(stat)(_ => gen.mkAttributedQualifier(rhs.tpe))

        // deferred val, trait val, lazy val (local or in class)
        case vd@ValDef(mods, name, tpt, rhs) if vd.symbol.hasFlag(ACCESSOR) && treeInfo.noFieldFor(vd, currOwner) =>
          val transformedRhs = atOwner(statSym)(transform(rhs))

          if (rhs == EmptyTree) mkAccessor(statSym)(EmptyTree)
          else if (currOwner.isTrait) mkAccessor(statSym)(castHack(transformedRhs, statSym.info.resultType))
          else if (!currOwner.isClass) mkLazyLocalDef(vd.symbol, transformedRhs)
          else {
            // TODO: make `synthAccessorInClass` a field and update it in atOwner?
            // note that `LazyAccessorTreeSynth` is pretty lightweight
            // (it's just a bunch of methods that all take a `clazz` parameter, which is thus stored as a field)
            val synthAccessorInClass = new SynthLazyAccessorsIn(currOwner)
            synthAccessorInClass.expandLazyClassMember(lazyVarOf(statSym), statSym, transformedRhs)
          }

        // drop the val for (a) constant (pure & not-stored) and (b) not-stored (but still effectful) fields
        case ValDef(mods, _, _, rhs) if (rhs ne EmptyTree) && !excludedAccessorOrFieldByFlags(statSym)
                                        && currOwner.isClass && fieldMemoizationIn(statSym, currOwner).constantTyped =>
          EmptyThicket

        case ModuleDef(_, _, impl) =>
          // ??? The typer doesn't take kindly to seeing this ClassDef; we have to set NoType so it will be ignored.
          val cd = super.transform(ClassDef(statSym.moduleClass, impl) setType NoType)
          if (currOwner.isClass) cd
          else { // local module -- symbols cannot be generated by info transformer, so do it all here
            val Block(stats, _) = mkLazyLocalDef(statSym, gen.newModule(statSym, statSym.info.resultType)): @unchecked

            Thicket(cd :: stats)
          }

        case tree =>
          super.transform(tree)

      }
    }


    def transformTermsAtExprOwner(exprOwner: Symbol)(stat: Tree) =
      if (stat.isTerm) atOwner(exprOwner)(transform(stat))
      else transform(stat)

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val addedStats =
        if (!currentOwner.isClass || currentOwner.isPackageClass) Nil
        else {
          val thickets = fieldsAndAccessors(currentOwner)
          if (thickets exists mustExplodeThicket)
            thickets flatMap explodeThicket
          else thickets
        }

      val newStats =
        stats mapConserve (if (exprOwner != currentOwner) transformTermsAtExprOwner(exprOwner) else transform)

      addedStats ::: (if (newStats eq stats) stats else {
        // check whether we need to flatten thickets and drop empty ones
        if (newStats exists mustExplodeThicket)
          newStats flatMap explodeThicket
        else newStats
      })
    }

  }
}
