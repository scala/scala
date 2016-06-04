/*  NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import scala.annotation.tailrec
import symtab.Flags._


/** Synthesize accessors and field for each (strict) val owned by a trait.
  *
  * For traits:
  *
  * - Namers translates a definition `val x = rhs` into a getter `def x = rhs` -- no underlying field is created.
  * - This phase synthesizes accessors and fields for any vals mixed into a non-trait class.
  * - Constructors will move the rhs to an assignment in the template body.
  *   Those statements then move to the template into the constructor,
  *   which means it will initialize the fields defined in this template (and execute the corresponding side effects).
  *   We need to maintain the connection between getter and rhs until after specialization so that it can duplicate vals.
  * - A ModuleDef is desugared to a ClassDef, an accessor (which reuses the module's term symbol)
  *   and a module var (unless the module is static and does not implement a member of a supertype, or we're in a trait).
  *   For subclasses of traits that define modules, a module var is mixed in, as well as the required module accessors.
  *
  * Runs after uncurry to deal with classes that implement SAM traits with ValDefs.
  * Runs before erasure (to get bridges), and thus before lambdalift/flatten, so that nested functions/definitions must be considered.
  *
  * We run after uncurry because it can introduce subclasses of traits with fields (SAMs with vals).
  * Lambdalift also introduces new fields (paramaccessors for captured vals), but runs too late in the pipeline
  * (mixins still synthesizes implementations for accessors that need to be mixed into subclasses of local traits that capture).
  *
  *
  * In the future, would like to get closer to dotty, which lifts a val's RHS (a similar thing is done for template-level statements)
  * to a method `$_initialize_$1$x` instead of a block, which is used in the constructor to initialize the val.
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
  * but the memoization overhead is removed when we statically know they are forced during initialiation.
  * We could still expose the low-level field semantics through `private[this] val`s.
  *
  * In any case, the current behavior of overriding vals is pretty surprising.
  * An overridden val's side-effect is still performed.
  * The only change due to overriding is that its value is never written to the field
  * (the overridden val's value is, of course, stored in the field in addition to its side-effect being performed).
  *
  * TODO: check init support (or drop the -Xcheck-init flag??)
  */
abstract class Fields extends InfoTransform with ast.TreeDSL with TypingTransformers {

  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "fields"

  protected def newTransformer(unit: CompilationUnit): Transformer = new FieldsTransformer(unit)
  override def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isJavaDefined || sym.isPackageClass || !sym.isClass) tp
    else synthFieldsAndAccessors(tp)

  // we leave lazy vars/accessors and early-init vals alone for now
  private def excludedAccessorOrFieldByFlags(statSym: Symbol): Boolean = statSym hasFlag LAZY | PRESUPER

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
  private def setMixedinAccessorFlags(orig: Symbol, cloneInSubclass: Symbol): Unit =
    cloneInSubclass setFlag OVERRIDE | NEEDS_TREES resetFlag DEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setFieldFlags(accessor: Symbol, fieldInSubclass: TermSymbol): Unit =
    fieldInSubclass setFlag (NEEDS_TREES |
                             PrivateLocal
                             | (accessor getFlag MUTABLE | LAZY)
                             | (if (accessor hasFlag STABLE) 0 else MUTABLE)
      )


  def checkAndClearOverriddenTraitSetter(setter: Symbol) = checkAndClear(OVERRIDDEN_TRAIT_SETTER)(setter)
  def checkAndClearNeedsTrees(setter: Symbol) = checkAndClear(NEEDS_TREES)(setter)
  def checkAndClear(flag: Long)(sym: Symbol) =
    sym.hasFlag(flag) match {
      case overridden =>
        sym resetFlag flag
        overridden
    }


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
    // not stored, no side-effect
    val pureConstant = tp.isInstanceOf[ConstantType]

    // if !stored, may still have a side-effect
    // (currently not distinguished -- used to think we could drop unit-typed vals,
    //  but the memory model cares about writes to unit-typed fields)
    val stored = !pureConstant // || isUnitType(tp))
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

  // drop field-targeting annotations from getters
  // (in traits, getters must also hold annotations that target the underlying field,
  //  because the latter won't be created until the trait is mixed into a class)
  // TODO do bean getters need special treatment to suppress field-targeting annotations in traits?
  def dropFieldAnnotationsFromGetter(sym: Symbol) =
    if (sym.isGetter && sym.owner.isTrait) {
      sym setAnnotations (sym.annotations filter AnnotationInfo.mkFilter(GetterTargetClass, defaultRetention = false))
    }


  // can't use the referenced field since it already tracks the module's moduleClass
  private[this] val moduleVarOf = perRunCaches.newMap[Symbol, Symbol]

  private def newModuleVarSymbol(site: Symbol, module: Symbol, tp: Type, extraFlags: Long): TermSymbol = {
//    println(s"new module var in $site for $module of type $tp")
    val moduleVar = site.newVariable(nme.moduleVarName(module.name.toTermName), module.pos.focus, MODULEVAR | extraFlags) setInfo tp addAnnotation VolatileAttr
    moduleVarOf(module) = moduleVar

    moduleVar
  }

  private def moduleInit(module: Symbol) = {
//    println(s"moduleInit for $module in ${module.ownerChain} --> ${moduleVarOf.get(module)}")
    val moduleVar = moduleVarOf(module)
    gen.mkAssignAndReturn(moduleVar, gen.newModule(module, moduleVar.info))
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

      moduleVarOf(accessor) = moduleVar

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
            val accessorUnderConsideration = !(member hasFlag (DEFERRED | LAZY))

            // destructively mangle accessor's name (which may cause rehashing of decls), also sets flags
            // this accessor has to be implemented in a subclass -- can't be private
            if ((member hasFlag PRIVATE) && fieldMemoization.stored) member makeNotPrivate clazz

            // This must remain in synch with publicizeTraitMethod in Mixins, so that the
            // synthesized member in a subclass and the trait member remain in synch regarding access.
            // Otherwise, the member will not be seen as overriding the trait member, and `addForwarders`'s call to
            // `membersBasedOnFlags` would see the deferred member in the trait, instead of the concrete (desired) one in the class
            // not doing: if (member hasFlag PROTECTED) member setFlag notPROTECTED

            // must not reset LOCAL, as we must maintain protected[this]ness to allow that variance hole
            // (not sure why this only problem only arose when we started setting the notPROTECTED flag)

            // derive trait setter after calling makeNotPrivate (so that names are mangled consistently)
            if (accessorUnderConsideration && fieldMemoization.stored) {
              synthesizeImplInSubclasses(member)

              if (member hasFlag STABLE) // TODO: check isGetter?
                newDecls += newTraitSetter(member, clazz)
            }
          } else if (member hasFlag MODULE) {
            nonStaticModuleToMethod(member)

            member setFlag NEEDS_TREES
            synthesizeImplInSubclasses(member)
          }
        }

        if (newDecls nonEmpty) {
          val allDecls = newScope
          origDecls foreach allDecls.enter
          newDecls  foreach allDecls.enter
          ClassInfoType(parents, allDecls, clazz)
        } else tp

      // mix in fields & accessors for all mixed in traits

      case tp@ClassInfoType(parents, oldDecls, clazz) if !clazz.isPackageClass =>
        val site = clazz.thisType

        // setter conflicts cannot arise independently from a getter conflict, since a setter without a getter does not a val definition make
        def accessorConflictsExistingVal(accessor: Symbol): Boolean = {
          val existingGetter = oldDecls.lookup(accessor.name.getterName)
//          println(s"$existingGetter from $accessor to ${accessor.name.getterName}")
          val tp = fieldTypeOfAccessorIn(accessor, site)
          (existingGetter ne NoSymbol) && (tp matches (site memberInfo existingGetter).resultType) // !existingGetter.isDeferred && -- see (3)
        }

        def newModuleVar(member: Symbol): TermSymbol =
          newModuleVarSymbol(clazz, member, site.memberType(member).resultType, PrivateLocal | SYNTHETIC | NEEDS_TREES)

        // a module does not need treatment here if it's static, unless it has a matching member in a superclass
        // a non-static method needs a module var
        val modulesNeedingExpansion =
          oldDecls.toList.filter(m => m.isModule && (!m.isStatic || m.isOverridingSymbol))

        // expand module def in class/object (if they need it -- see modulesNeedingExpansion above)
        val expandedModules =
          modulesNeedingExpansion map { module =>
            // expanding module def (top-level or nested in static module)
            if (module.isStatic) { // implies m.isOverridingSymbol as per above filter
              // Need a module accessor, to implement/override a matching member in a superclass.
              // Never a need for a module var if the module is static.
              newMatchingModuleAccessor(clazz, module)
            } else {
              nonStaticModuleToMethod(module)
              // must reuse symbol instead of creating an accessor
              module setFlag NEEDS_TREES
              newModuleVar(module)
            }
          }

//        println(s"expanded modules for $clazz: $expandedModules")

        // afterOwnPhase, so traits receive trait setters for vals (needs to be at finest grain to avoid looping)
        val synthInSubclass =
          clazz.mixinClasses.flatMap(mixin => afterOwnPhase{mixin.info}.decls.toList.filter(accessorImplementedInSubclass))

        // mixin field accessors --
        // invariant: (accessorsMaybeNeedingImpl, mixedInAccessorAndFields).zipped.forall(case (acc, clone :: _) => `clone` is clone of `acc` case _ => true)
        val mixedInAccessorAndFields = synthInSubclass.map{ member =>
          def cloneAccessor() = {
            val clonedAccessor = (member cloneSymbol clazz) setPos clazz.pos
            setMixedinAccessorFlags(member, clonedAccessor)

            if (clonedAccessor.isGetter)
              clonedAccessor setAnnotations (clonedAccessor.annotations filter AnnotationInfo.mkFilter(GetterTargetClass, defaultRetention = false))

            // if we don't cloneInfo, method argument symbols are shared between trait and subclasses --> lambalift proxy crash
            // TODO: use derive symbol variant?
//            println(s"cloning accessor $member to $clazz")
            // start at uncurry so that we preserve that part of the history where an accessor has a NullaryMethodType
            enteringUncurry { clonedAccessor setInfo ((clazz.thisType memberType member) cloneInfo clonedAccessor) }
            clonedAccessor
          }

          if (member hasFlag MODULE) {
            val moduleVar = newModuleVar(member)
            List(moduleVar, newModuleAccessor(member, clazz, moduleVar))
          }
          // when considering whether to mix in the trait setter, forget about conflicts -- they are reported for the getter
          // a trait setter for an overridden val will receive a unit body in the tree transform
          else if (nme.isTraitSetterName(member.name)) {
            val getter = member.getterIn(member.owner)
            val clone = cloneAccessor()
            
            setClonedTraitSetterFlags(clazz, getter, clone)
            // println(s"mixed in trait setter ${clone.defString}")

            List(clone)
          }
          // don't cause conflicts, skip overridden accessors contributed by supertraits (only act on the last overriding one)
          // see pos/trait_fields_dependent_conflict.scala and neg/t1960.scala
          else if (accessorConflictsExistingVal(member) || isOverriddenAccessor(member, clazz)) Nil
          else if (member.isGetter && fieldMemoizationIn(member, clazz).stored) {
            // add field if needed
            val field = clazz.newValue(member.localName, member.pos) setInfo fieldTypeForGetterIn(member, clazz.thisType)

            setFieldFlags(member, field)

            // filter getter's annotations to exclude those only meant for the field
            // we must keep them around long enough to see them here, though, when we create the field
            field setAnnotations (member.annotations filter AnnotationInfo.mkFilter(FieldTargetClass, defaultRetention = true))

            List(cloneAccessor(), field)
          } else List(cloneAccessor()) // no field needed (constant-typed getter has constant as its RHS)
        }

//        println(s"mixedInAccessorAndFields for $clazz: $mixedInAccessorAndFields")

        // omit fields that are not memoized, retain all other members
        def omittableField(sym: Symbol) = sym.isValue && !sym.isMethod && !fieldMemoizationIn(sym, clazz).stored

        val newDecls =
          if (expandedModules.isEmpty && mixedInAccessorAndFields.isEmpty) oldDecls.filterNot(omittableField)
          else {
            // must not alter `decls` directly
            val newDecls = newScope
            val enter    = newDecls enter (_: Symbol)
            val enterAll = (_: List[Symbol]) foreach enter

            oldDecls foreach { d => if (!omittableField(d)) enter(d) }
            expandedModules foreach enter
            mixedInAccessorAndFields foreach enterAll

            newDecls
          }

//        println(s"new decls for $clazz: $expandedModules ++ $mixedInAccessorAndFields")

        if (newDecls eq oldDecls) tp
        else ClassInfoType(parents, newDecls, clazz)

      case tp => mapOver(tp)
    }
  }


  // done by uncurry's info transformer
  // instead of forcing every member's info to run said transformer, duplicate the flag update logic...
  def nonStaticModuleToMethod(module: Symbol): Unit = {
    if (!module.isStatic) module setFlag METHOD | STABLE
  }

  class FieldsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def mkTypedUnit(pos: Position) = localTyper.typedPos(pos)(CODE.UNIT)
    def deriveUnitDef(stat: Tree)  = deriveDefDef(stat)(_ => mkTypedUnit(stat.pos))

    def mkAccessor(accessor: Symbol)(body: Tree) = localTyper.typedPos(accessor.pos)(DefDef(accessor, body)).asInstanceOf[DefDef]

    def mkField(sym: Symbol) = localTyper.typedPos(sym.pos)(ValDef(sym)).asInstanceOf[ValDef]


    // synth trees for accessors/fields and trait setters when they are mixed into a class
    def fieldsAndAccessors(clazz: Symbol): List[ValOrDefDef] = {
      def fieldAccess(accessor: Symbol): Option[Tree] = {
        val fieldName = accessor.localName
        val field = clazz.info.decl(fieldName)
        // The `None` result denotes an error, but it's refchecks' job to report it (this fallback is for robustness).
        // This is the result of overriding a val with a def, so that no field is found in the subclass.
        if (field.exists) Some(Select(This(clazz), field))
        else None
      }

      def getterBody(getter: Symbol): Option[Tree] = {
        // accessor created by newMatchingModuleAccessor for a static module that does need an accessor
        // (because there's a matching member in a super class)
        if (getter.asTerm.referenced.isModule) {
          Some(gen.mkAttributedRef(clazz.thisType, getter.asTerm.referenced))
        } else {
          val fieldMemoization = fieldMemoizationIn(getter, clazz)
          if (fieldMemoization.pureConstant) Some(gen.mkAttributedQualifier(fieldMemoization.tp)) // TODO: drop when we no longer care about producing identical bytecode
          else fieldAccess(getter)
        }
      }

      //      println(s"accessorsAndFieldsNeedingTrees for $templateSym: $accessorsAndFieldsNeedingTrees")
      def setterBody(setter: Symbol): Option[Tree] = {
        // trait setter in trait
        if (clazz.isTrait) Some(EmptyTree)
        // trait setter for overridden val in class
        else if (checkAndClearOverriddenTraitSetter(setter)) Some(mkTypedUnit(setter.pos))
        // trait val/var setter mixed into class
        else fieldAccess(setter) map (fieldSel => Assign(fieldSel, Ident(setter.firstParam)))
      }

      def moduleAccessorBody(module: Symbol): Some[Tree] = Some(
        // added during synthFieldsAndAccessors using newModuleAccessor
        // a module defined in a trait by definition can't be static (it's a member of the trait and thus gets a new instance for every outer instance)
        if (clazz.isTrait) EmptyTree
        // symbol created by newModuleAccessor for a (non-trait) class
        else moduleInit(module)
      )

      clazz.info.decls.toList.filter(checkAndClearNeedsTrees) flatMap {
        case module if module hasAllFlags (MODULE | METHOD) => moduleAccessorBody(module) map mkAccessor(module)
        case setter if setter.isSetter                      => setterBody(setter) map mkAccessor(setter)
        case getter if getter.hasFlag(ACCESSOR)             => getterBody(getter) map mkAccessor(getter)
        case field  if !(field hasFlag METHOD)              => Some(mkField(field)) // vals/vars and module vars (cannot have flags PACKAGE | JAVA since those never receive NEEDS_TREES)
        case _ => None
      }
    }

    def rhsAtOwner(stat: ValOrDefDef, newOwner: Symbol): Tree =
      atOwner(newOwner)(super.transform(stat.rhs.changeOwner(stat.symbol -> newOwner)))


    private def Thicket(trees: List[Tree]) = Block(trees, EmptyTree)
    override def transform(stat: Tree): Tree = {
      val clazz = currentOwner
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
                                           && !clazz.isTrait // we've already done this for traits.. the asymmetry will be solved by the above todo
                                           && fieldMemoizationIn(statSym, clazz).pureConstant =>
          deriveDefDef(stat)(_ => gen.mkAttributedQualifier(rhs.tpe)) // TODO: recurse?

        // drop the val for (a) constant (pure & not-stored) and (b) not-stored (but still effectful) fields
        case ValDef(mods, _, _, rhs) if (rhs ne EmptyTree) && !excludedAccessorOrFieldByFlags(statSym)
                                        && fieldMemoizationIn(statSym, clazz).pureConstant =>
          EmptyTree

        case ModuleDef(_, _, impl) =>
          // ??? The typer doesn't take kindly to seeing this ClassDef; we have to set NoType so it will be ignored.
          val cd = super.transform(ClassDef(statSym.moduleClass, impl) setType NoType)
          if (clazz.isClass) cd
          else { // local module -- symbols cannot be generated by info transformer, so do it all here
            val moduleVar = newModuleVarSymbol(currentOwner, statSym, statSym.info.resultType, 0)
            Thicket(cd :: mkField(moduleVar) :: mkAccessor(statSym)(moduleInit(statSym)) :: Nil)
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
        if (!currentOwner.isClass) Nil
        else afterOwnPhase { fieldsAndAccessors(currentOwner) }

      val newStats =
        stats mapConserve (if (exprOwner != currentOwner) transformTermsAtExprOwner(exprOwner) else transform)

      addedStats ::: (if (newStats eq stats) stats else {
        // check whether we need to flatten thickets and drop empty ones
        if (newStats exists { case EmptyTree => true case Block(_, EmptyTree) => true case _ => false })
          newStats flatMap {
            case EmptyTree => Nil
            case Block(thicket, EmptyTree) => thicket
            case stat => stat :: Nil
          }
        else newStats
      })
    }

  }
}
