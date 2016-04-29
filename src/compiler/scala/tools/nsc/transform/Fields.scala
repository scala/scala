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
  *   - Namers translates a definition `val x = rhs` into a getter `def x = rhs` -- no underlying field is created.
  *   - This phase synthesizes accessors and fields for any vals mixed into a non-trait class.
  *   - Constructors will move the rhs to an assignment in the template body.
  *     and those statements then move to the template into the constructor,
  *     which means it will initialize the fields defined in this template (and execute the corresponding side effects).
  *     We need to maintain the connection between getter and rhs until after specialization so that it can duplicate vals.
  *
  * Runs before erasure (to get bridges), and thus before lambdalift/flatten, so that nested functions/definitions must be considered.
  * We run after uncurry because it can introduce subclasses of traits with fields (SAMs with vals).
  * Lambdalift also introduces new fields (paramaccessors for captured vals), but runs too late in the pipeline
  * (mixins still synthesizes implementations for accessors that need to be mixed into subclasses of local traits that capture).
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
  *  ideally, we'd avoid renumbering unchanged methods, but that would result in
  *  different bytecode between clean recompiles and incremental ones).
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

  private final val OVERRIDDEN_TRAIT_SETTER = TRANS_FLAG

  final val TRAIT_SETTER_FLAGS = NEEDS_TREES | DEFERRED | ProtectedLocal

  private def accessorImplementedInSubclass(accessor: Symbol) =
    (accessor hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS) && (accessor hasFlag (ACCESSOR))

  private def concreteOrSynthImpl(sym: Symbol): Boolean = !(sym hasFlag DEFERRED) || (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)

  private def synthesizeImplInSubclasses(accessor: Symbol): Unit =
    accessor setFlag lateDEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setClonedTraitSetterFlags(clazz: Symbol, correspondingGetter: Symbol, cloneInSubclass: Symbol): Unit = {
    val overridden = isOverriddenAccessor(correspondingGetter, clazz)
    if (overridden) cloneInSubclass setFlag OVERRIDDEN_TRAIT_SETTER
    else if (correspondingGetter.isEffectivelyFinal) cloneInSubclass setFlag FINAL
  }

  // TODO: add MIXEDIN (see e.g., `accessed` on `Symbol`)
  private def setMixedinAccessorFlags(orig: Symbol, cloneInSubclass: Symbol): Unit =
    cloneInSubclass setFlag OVERRIDE | NEEDS_TREES resetFlag DEFERRED | lateDEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setFieldFlags(accessor: Symbol, fieldInSubclass: TermSymbol): Unit =
    fieldInSubclass setFlag (NEEDS_TREES |
                             PrivateLocal
                             | (accessor getFlag MUTABLE | LAZY)
                             | (if (accessor hasFlag STABLE) 0 else MUTABLE)
      )


  def checkAndClearOverridden(setter: Symbol) = checkAndClear(OVERRIDDEN_TRAIT_SETTER)(setter)
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
    val res = member.matchingSymbol(clazz, pre) filter (sym => (sym hasFlag ACCESSOR) && concreteOrSynthImpl(sym))
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
            if (member hasFlag PRIVATE) member makeNotPrivate clazz

            // Need to mark as notPROTECTED, so that it's carried over to the synthesized member in subclasses,
            // since the trait member will receive this flag later in ExplicitOuter, but the synthetic subclass member will not.
            // If we don't add notPROTECTED to the synthesized one, the member will not be seen as overriding the trait member.
            // Therefore, addForwarders's call to membersBasedOnFlags would see the deferred member in the trait,
            // instead of the concrete (desired) one in the class
            // TODO: encapsulate as makeNotProtected, similar to makeNotPrivate (also do moduleClass, e.g.)
            if (member hasFlag PROTECTED) member setFlag notPROTECTED

            // must not reset LOCAL, as we must maintain protected[this]ness to allow that variance hole
            // (not sure why this only problem only arose when we started setting the notPROTECTED flag)

            // derive trait setter after calling makeNotPrivate (so that names are mangled consistently)
            if (accessorUnderConsideration && fieldMemoization.stored) {
              synthesizeImplInSubclasses(member)

              if (member hasFlag STABLE) // TODO: check isGetter?
                newDecls += newTraitSetter(member, clazz)
            }
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
        // TODO (1): improve logic below, which is used to avoid mixing in anything that would result in an error in refchecks
        // (a reason to run after refchecks? we should run before pickler, though, I think, so that the synthesized stats are pickled)

        val membersNeedingSynthesis = clazz.mixinClasses.flatMap { mixin =>
          // afterOwnPhase, so traits receive trait setters for vals
          afterOwnPhase {mixin.info}.decls.toList.filter(accessorImplementedInSubclass)
        }

//        println(s"mixing in for $clazz: $membersNeedingSynthesis from ${clazz.mixinClasses}")

        // TODO: setter conflicts?
        def accessorConflictsExistingVal(accessor: Symbol): Boolean = {
          val existingGetter = oldDecls.lookup(accessor.name.getterName)
          //          println(s"$existingGetter from $accessor to ${accessor.name.getterName}")
          val tp = fieldTypeOfAccessorIn(accessor, site)
          (existingGetter ne NoSymbol) && (tp matches (site memberInfo existingGetter).resultType) // !existingGetter.isDeferred && -- see (3)
        }

        // mixin field accessors --
        // invariant: (accessorsMaybeNeedingImpl, mixedInAccessorAndFields).zipped.forall(case (acc, clone :: _) => `clone` is clone of `acc` case _ => true)
        val synthAccessorAndFields = membersNeedingSynthesis map { member =>
          def cloneAccessor() = {
            val clonedAccessor = (member cloneSymbol clazz) setPos clazz.pos
            setMixedinAccessorFlags(member, clonedAccessor)

            if (clonedAccessor.isGetter)
              clonedAccessor setAnnotations (clonedAccessor.annotations filter AnnotationInfo.mkFilter(GetterTargetClass, defaultRetention = false))

            // if we don't cloneInfo, method argument symbols are shared between trait and subclasses --> lambalift proxy crash
            // TODO: use derive symbol variant?
            //            println(s"cloning accessor $accessor to $clazz / $clonedInfo -> $relativeInfo")
            clonedAccessor setInfo ((clazz.thisType memberType member) cloneInfo clonedAccessor) // accessor.info.cloneInfo(clonedAccessor).asSeenFrom(clazz.thisType, accessor.owner)
          }

          // when considering whether to mix in the trait setter, forget about conflicts -- they will be reported for the getter
          // a trait setter for an overridden val will receive a unit body in the tree transform
          if (nme.isTraitSetterName(member.name)) {
            val getter = member.getterIn(member.owner)
            val clone = cloneAccessor()

            clone filterAnnotations (ai => !ai.matches(TraitSetterAnnotationClass)) // only supposed to be set in trait

            setClonedTraitSetterFlags(clazz, getter, clone)
            // println(s"mixed in trait setter ${clone.defString}")

            List(clone)
          }
          // avoid creating early errors in case of conflicts (wait until refchecks);
          // also, skip overridden accessors contributed by supertraits (only act on the last overriding one)
          else if (accessorConflictsExistingVal(member) || isOverriddenAccessor(member, clazz)) Nil
          else if (member.isGetter && fieldMemoizationIn(member, clazz).stored) {
            // add field if needed
            val field = clazz.newValue(member.localName, member.pos) setInfo fieldTypeForGetterIn(member, clazz.thisType)

            setFieldFlags(member, field)

            // filter getter's annotations to exclude those only meant for the field
            // we must keep them around long enough to see them here, though, when we create the field
            field setAnnotations (member.annotations filter AnnotationInfo.mkFilter(FieldTargetClass, defaultRetention = true))

            List(cloneAccessor(), field)
          } else List(cloneAccessor())
        }

        //        println(s"new decls for $clazz: $mixedInAccessorAndFields")

        // omit fields that are not memoized, retain all other members
        def omittableField(sym: Symbol) = sym.isValue && !sym.isMethod && !fieldMemoizationIn(sym, clazz).stored

        val newDecls =
          if (synthAccessorAndFields.isEmpty) oldDecls.filterNot(omittableField)
          else {
            // must not alter `decls` directly
            val newDecls = newScope
            val enter    = newDecls enter (_: Symbol)
            val enterAll = (_: List[Symbol]) foreach enter

            oldDecls foreach { d => if (!omittableField(d)) enter(d) }
            synthAccessorAndFields foreach enterAll

            newDecls
          }

        //        println(s"new decls: $newDecls")

        if (newDecls eq oldDecls) tp
        else ClassInfoType(parents, newDecls, clazz)

      case tp => mapOver(tp)
    }
  }



  class FieldsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def mkTypedUnit(pos: Position) = localTyper.typedPos(pos)(CODE.UNIT)
    def deriveUnitDef(stat: Tree)  = deriveDefDef(stat)(_ => mkTypedUnit(stat.pos))

    def mkAccessor(accessor: Symbol)(body: Tree) = localTyper.typedPos(accessor.pos)(DefDef(accessor, body)).asInstanceOf[DefDef]

    def mkField(sym: Symbol) = localTyper.typedPos(sym.pos)(ValDef(sym)).asInstanceOf[ValDef]


    // synth trees for accessors/fields and trait setters when they are mixed into a class
    def fieldsAndAccessors(exprOwner: Symbol): List[ValOrDefDef] = {
      if (exprOwner.isLocalDummy) {
        val clazz = exprOwner.owner
        def fieldAccess(accessor: Symbol): Option[Tree] = {
          val fieldName = accessor.localName
          val field = clazz.info.decl(fieldName)
          // The `None` result denotes an error, but we defer to refchecks to report it.
          // This is the result of overriding a val with a def, so that no field is found in the subclass.
          if (field.exists) Some(Select(This(clazz), field))
          else None
        }

        def getterBody(getter: Symbol): Option[Tree] = {
          val fieldMemoization = fieldMemoizationIn(getter, clazz)
          if (fieldMemoization.pureConstant) Some(gen.mkAttributedQualifier(fieldMemoization.tp)) // TODO: drop when we no longer care about producing identical bytecode
          else fieldAccess(getter)
        }

        //      println(s"accessorsAndFieldsNeedingTrees for $templateSym: $accessorsAndFieldsNeedingTrees")
        def setterBody(setter: Symbol): Option[Tree] = {
          // trait setter in trait
          if (clazz.isTrait) Some(EmptyTree)
          // trait setter for overridden val in class
          else if (checkAndClearOverridden(setter)) Some(mkTypedUnit(setter.pos))
          // trait val/var setter mixed into class
          else fieldAccess(setter) map (fieldSel => Assign(fieldSel, Ident(setter.firstParam)))
        }


        clazz.info.decls.toList.filter(checkAndClearNeedsTrees) flatMap {
          case setter if setter.isSetter                      => setterBody(setter) map mkAccessor(setter)
          case getter if getter.isAccessor                    => getterBody(getter) map mkAccessor(getter)
          case field  if !(field hasFlag METHOD)              => Some(mkField(field)) // vals/vars and module vars (cannot have flags PACKAGE | JAVA since those never receive NEEDS_TREES)
          case _ => None
        }
      } else {
//        println(s"$exprOwner : ${exprOwner.info} --> ${exprOwner.info.decls}")
        Nil
      }
    }

    def rhsAtOwner(stat: ValOrDefDef, newOwner: Symbol): Tree =
      atOwner(newOwner)(super.transform(stat.rhs.changeOwner(stat.symbol -> newOwner)))

    private def transformStat(exprOwner: Symbol)(stat: Tree): List[Tree] = {
      val clazz = currentOwner
      val statSym = stat.symbol

      //       println(s"transformStat $statSym in ${exprOwner.ownerChain}")
      //        currentRun.trackerFactory.snapshot()

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
        case stat@DefDef(_, _, _, _, _, rhs) if (statSym hasFlag ACCESSOR) && !excludedAccessorOrFieldByFlags(statSym) =>
          /* TODO: defer replacing ConstantTyped tree by the corresponding constant until erasure
             (until then, trees should not be constant-folded -- only their type tracks the resulting constant)
             TODO: also remove ACCESSOR flag since there won't be an underlying field to access?
          */
          def statInlinedConstantRhs =
            if (clazz.isTrait) stat // we've already done this for traits.. the asymmetry will be solved by the above todo
            else deriveDefDef(stat)(_ => gen.mkAttributedQualifier(rhs.tpe))

          if (rhs ne EmptyTree) {
            val fieldMemoization = fieldMemoizationIn(statSym, clazz)

            // if we decide to have non-stored fields with initialization effects, the stat's RHS should be replaced by unit
            // if (!fieldMemoization.stored) deriveUnitDef(stat) else stat

            if (fieldMemoization.pureConstant) statInlinedConstantRhs :: Nil
            else super.transform(stat) :: Nil
          } else {
            stat :: Nil
          }

        case stat@ValDef(mods, _, _, rhs) if !excludedAccessorOrFieldByFlags(statSym) =>
          if (rhs ne EmptyTree) {
            val fieldMemoization = fieldMemoizationIn(statSym, clazz)

            // drop the val for (a) constant (pure & not-stored) and (b) not-stored (but still effectful) fields
            if (fieldMemoization.pureConstant) Nil // (a)
            else super.transform(stat) :: Nil // if (fieldMemoization.stored)
            //            else rhsAtOwner(transformStat, exprOwner) :: Nil // (b) -- not used currently
          } else {
            stat :: Nil
          }


        case tree => List(
          if (exprOwner != currentOwner && tree.isTerm) atOwner(exprOwner)(super.transform(tree))
          else super.transform(tree)
        )
      }
    }

    // TODO flatMapConserve or something like it
    // TODO use thicket encoding of multi-tree transformStat?
    // if (!currentOwner.isClass || currentOwner.isPackageClass || currentOwner.isInterface) stats flatMap transformStat(exprOwner) // for the ModuleDef case, the only top-level case in that method
    // else
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      afterOwnPhase {
        fieldsAndAccessors(exprOwner) ++ (stats flatMap transformStat(exprOwner))
      }
  }
}
