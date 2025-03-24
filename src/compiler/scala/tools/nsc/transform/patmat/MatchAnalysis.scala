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

package scala.tools.nsc.transform.patmat

import scala.annotation._
import scala.collection.mutable
import scala.tools.nsc.Reporting.WarningCategory

trait TreeAndTypeAnalysis extends Debugging {
  import global._
  import definitions._
  import analyzer.Typer

  /** Compute the type T implied for a value `v` matched by a pattern `pat` (with expected type `pt`).
   *
   *  Usually, this is the pattern's type because pattern matching implies instance-of checks.
   *
   *  However, Stable Identifier and Literal patterns are matched using `==`,
   *  which does not imply a type for the binder that binds the matched value.
   *  E.g., in `case x@Nil => x `, all we know about `x` is that it satisfies `Nil == x`, which could be anything.
   *  A type pattern with a literal type works the same as the corresponding literal pattern.
   *  A literal pattern with a Boolean or Unit pattern does enforce that the respective value (`true`, `false`, `()`)
   *  was matched, so in those cases, the pattern type is assumed.
   *
   *  The other patterns imply type tests, so we can safely deduce that the binder has
   *  the pattern's type when the pattern matches.
   *  Concretely, a literal, type pattern, a case class (the constructor's result type)
   *  or extractor (the unapply's argument type) all imply type tests.
   *
   *  See scala/bug#1503, scala/bug#5024: don't cast binders to types we're not sure they have
   */
  def binderTypeImpliedByPattern(pat: Tree, pt: Type): Type =
    pat match {
      case _ if pat.tpe <:< BooleanTpe || pat.tpe <:< UnitTpe || pat.tpe <:< StringTpe
                                                              => pat.tpe
      case Ident(_) | Select(_, _) | Literal(_)               => pt
      case Typed(_, _) if pat.tpe.isInstanceOf[ConstantType]  => pt
      case _                                                  => pat.tpe
    }

  // we use subtyping as a model for implication between instanceof tests
  // i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
  // unfortunately this is not true in general:
  // scala/bug#6022 expects instanceOfTpImplies(ProductClass.tpe, AnyRefTpe)
  def instanceOfTpImplies(tp: Type, tpImplied: Type) = {
    val tpValue = isPrimitiveValueType(tp)

    // pretend we're comparing to Any when we're actually comparing to AnyVal or AnyRef
    // (and the subtype is respectively a value type or not a value type)
    // this allows us to reuse subtyping as a model for implication between instanceOf tests
    // the latter don't see a difference between AnyRef, Object or Any when comparing non-value types -- scala/bug#6022
    val tpImpliedNormalizedToAny =
      if (tpImplied =:= (if (tpValue) AnyValTpe else AnyRefTpe)) AnyTpe
      else tpImplied

    tp <:< tpImpliedNormalizedToAny
  }

  def equivalentTree(a: Tree, b: Tree): Boolean = (a, b) match {
    case (Select(qual1, _), Select(qual2, _))             => equivalentTree(qual1, qual2) && a.symbol == b.symbol
    case (Ident(_), Ident(_))                             => a.symbol == b.symbol
    case (Literal(c1), Literal(c2))                       => c1 == c2
    case (This(_), This(_))                               => a.symbol == b.symbol
    case (Apply(fun1, args1), Apply(fun2, args2))         => equivalentTree(fun1, fun2) && args1.corresponds(args2)(equivalentTree)
    case (TypeApply(fun1, args1), TypeApply(fun2, args2)) => equivalentTree(fun1, fun2) && args1.corresponds(args2)(equivalentTree)
    case (a @ TypeTree(), b @ TypeTree())                 => a.tpe =:= b.tpe
    case _                                                => false // Those are the only cases we need to handle in the pattern matcher
  }

  trait CheckableTreeAndTypeAnalysis {
    val typer: Typer

    def enumerateSubtypes(tp: Type, grouped: Boolean): List[List[Type]] = tp.typeSymbol match {
      case UnitClass                    => List(List(UnitTpe))
      case BooleanClass                 => List(List(ConstantTrue, ConstantFalse))
      case sym if sym.isModuleClass     => List(List(tp))
      case sym if sym.isRefinementClass => enumerateRefinement(tp, grouped)
      case sym if sym.isSealed          => enumerateSealed(tp, grouped)
      case sym if sym.isCase            => List(List(tp))
      case sym if sym.isTypeSkolem      => enumerateSubtypes(sym.info.upperBound, grouped) // pos/t12277
      case sym                          => debug.patmatResult(s"enum unsealed tp=$tp sym=$sym")(Nil)
    }

    private def enumerateRefinement(tp: Type, grouped: Boolean) = {
      val parentSubtypes = tp.parents.flatMap(parent => enumerateSubtypes(parent, grouped))
      if (parentSubtypes.exists(_.nonEmpty)) {
        // If any of the parents is enumerable, then the refinement type is enumerable.
        // We must only include subtypes of the parents that conform to `tpApprox`.
        // See neg/virtpatmat_exhaust_compound.scala and pos/t9657.scala for examples.
        val approximateTypeSkolemsToUpperBound = new TypeMap { // from approximateAbstracts
          def apply(tp: Type): Type = tp.dealiasWiden match {
            case TypeRef(_, sym, _) if sym.isTypeSkolem => tp.upperBound
            case _                                      => mapOver(tp)
          }
        }
        val tpApprox = approximateTypeSkolemsToUpperBound(tp)
        parentSubtypes.map(_.filter(_ <:< tpApprox))
      } else Nil
    }

    private def enumerateSealed(tp: Type, grouped: Boolean): List[List[Type]] = {
      val tpApprox = analyzer.approximateAbstracts(tp)
      val pre      = tp.prefix
      val sym      = tp.typeSymbol

      def subclassesToSubtypes(syms: List[Symbol]): List[Type] = syms.flatMap { sym =>
        // have to filter out children which cannot match: see ticket #3683 for an example
        // compare to the fully known type `tp` (modulo abstract types),
        // so that we can rule out stuff like:
        //    sealed trait X[T]; class XInt extends X[Int]
        // XInt not valid when enumerating X[String]
        // however, must also approximate abstract types
        val memberType  = nestedMemberType(sym, pre, tp.typeSymbol.owner)
        val subTp       = appliedType(memberType, WildcardType.fillList(sym.typeParams.length))
        val subTpApprox = analyzer.approximateAbstracts(subTp)
        if (subTpApprox <:< tpApprox) Some(checkableType(subTp)) else None
      }

      def filterAndSortChildren(children: Set[Symbol]) = {
        // symbols which are both sealed and abstract need not be covered themselves,
        // because all of their children must be and they cannot otherwise be created.
        val children1 = children.toList
          .filterNot(child => child.isSealed && (child.isAbstractClass || child.hasJavaEnumFlag))
          .sortBy(_.sealedSortName)
        children1.filterNot { child =>
          // remove private abstract children that are superclasses of other children, for example in t6159 drop X2
          child.isPrivate && child.isAbstractClass && children1.exists(sym => (sym ne child) && sym.isSubClass(child))
        }
      }

      @tailrec def groupChildren(wl: List[Symbol], acc: List[List[Symbol]]): List[List[Symbol]] = wl match {
        case Nil      => acc
        case hd :: tl =>
          val children = filterAndSortChildren(hd.sealedChildren)
          // put each trait in a new group since traits could belong to the same group as a derived class
          val (traits, nonTraits) = children.partition(_.isTrait)
          groupChildren(tl ::: children, acc ::: traits.map(List(_)).appended(nonTraits))
      }

      val subclasses = debug.patmatResult(s"enum $sym sealed, subclasses") {
        if (grouped) groupChildren(List(sym), Nil)
        else List(filterAndSortChildren(sym.sealedDescendants))
      }

      debug.patmatResult(s"enum $sym sealed tp=$tp tpApprox=$tpApprox, subtypes") {
        // A valid subtype is turned into a checkable type, as we are entering the realm of the dynamic
        subclasses.map(subclassesToSubtypes)
      }
    }

    // approximate a type to the static type that is fully checkable at run time,
    // hiding statically known but dynamically uncheckable information using existential quantification
    // TODO: this is subject to the availability of TypeTags (since an abstract type with a type tag is checkable at run time)
    def checkableType(tp: Type): Type = {
      // TODO: this is extremely rough...
      // replace type args by wildcards, since they can't be checked (don't use existentials: overkill)
      // TODO: when type tags are available, we will check -- when this is implemented, can we take that into account here?
      // similar to typer.infer.approximateAbstracts
      object typeArgsToWildcardsExceptArray extends TypeMap {
        // scala/bug#6771 dealias would be enough today, but future proofing with the dealiasWiden.
        // See neg/t6771b.scala for elaboration
        def apply(tp: Type): Type = tp.dealias match {
          case TypeRef(pre, sym, args) if args.nonEmpty && (sym ne ArrayClass) =>
            TypeRef(pre, sym, WildcardType.fillList(args.length))
          case _ =>
            mapOver(tp)
        }
      }
      debug.patmatResult(s"checkableType($tp)")(typeArgsToWildcardsExceptArray(tp))
    }

    // A type is "uncheckable" (for exhaustivity) if we don't statically know its subtypes (i.e., it's unsealed)
    // A tuple of all uncheckable types is uncheckable
    def uncheckableType(tp: Type): Boolean = {
      if (isTupleType(tp)) tupleComponents(tp).forall(uncheckableType)
      else enumerateSubtypes(tp, grouped = false).isEmpty
    }
  }
}

trait MatchApproximation extends TreeAndTypeAnalysis with ScalaLogic with MatchTreeMaking {
  import global._
  import global.definitions._

  /**
   * Represent a match as a formula in propositional logic that encodes whether the match matches (abstractly: we only consider types)
   *
   */
  trait MatchApproximator extends TreeMakers with TreesAndTypesDomain {
    object Test {
      var currId = 0
    }
    case class Test(prop: Prop, treeMaker: TreeMaker) {
      val id = { Test.currId += 1; Test.currId}
      override def toString = s"T${id}C($prop)"
    }

    // returns (tree, tests), where `tree` will be used to refer to `root` in `tests`
    class TreeMakersToProps(val root: Symbol) {
      prepareNewAnalysis() // reset hash consing for Var and Const

      private[this] val uniqueEqualityProps = new mutable.HashMap[(Tree, Tree), Eq]
      private[this] val uniqueNonNullProps  = new mutable.HashMap[Tree, Not]
      private[this] val uniqueTypeProps     = new mutable.HashMap[(Tree, Type), Eq]

      def uniqueEqualityProp(testedPath: Tree, rhs: Tree): Prop =
        uniqueEqualityProps.getOrElseUpdate((testedPath, rhs), Eq(Var(testedPath), ValueConst(rhs)))

      def uniqueNonNullProp (testedPath: Tree): Prop =
        uniqueNonNullProps.getOrElseUpdate(testedPath, Not(Eq(Var(testedPath), NullConst)))

      def uniqueTypeProp(testedPath: Tree, pt: Type): Prop =
        uniqueTypeProps.getOrElseUpdate((testedPath, pt), Eq(Var(testedPath), TypeConst(checkableType(pt))))

      // a variable in this set should never be replaced by a tree that "does not consist of a selection on a variable in this set" (intuitively)
      private val pointsToBound  = mutable.HashSet(root)
      private val trees          = mutable.HashSet.empty[Tree]
      private val extractBinders = mutable.HashMap.empty[Tree, Symbol]

      // the substitution that renames variables to variables in pointsToBound
      private var normalize: Substitution  = EmptySubstitution
      private var substitutionComputed = false

      // replaces a variable (in pointsToBound) by a selection on another variable in pointsToBound
      // in the end, instead of having x1, x1.hd, x2, x2.hd, ... flying around,
      // we want something like x1, x1.hd, x1.hd.tl, x1.hd.tl.hd, so that we can easily recognize when
      // we're testing the same variable
      // TODO check:
      //   pointsToBound -- accumSubst.from == Set(root) && (accumSubst.from.toSet -- pointsToBound) isEmpty
      private var accumSubst: Substitution = EmptySubstitution

      // hashconsing trees (modulo value-equality)
      def unique(t: Tree, tpOverride: Type = NoType): Tree =
        trees find (a => equivalentTree(a, t)) match {
          case Some(orig) =>
            // debug.patmat("unique: "+ (t eq orig, orig))
            orig
          case _ =>
            trees += t
            if (tpOverride != NoType) t setType tpOverride
            else t
        }

      def uniqueTp(tp: Type): Type = tp match {
        // typerefs etc are already hashconsed
        case _ : UniqueType                      => tp
        case tp@RefinedType(parents, EmptyScope) => tp.memo(tp: Type)(identity) // TODO: does this help?
        case _                                   => tp
      }

      // produce the unique tree used to refer to this binder
      // the type of the binder passed to the first invocation
      // determines the type of the tree that'll be returned for that binder as of then
      final def binderToUniqueTree(b: Symbol) =
        unique(accumSubst(normalize(gen.mkAttributedStableRef(b))), b.tpe)

      // note that the sequencing of operations is important: must visit in same order as match execution
      // binderToUniqueTree uses the type of the first symbol that was encountered as the type for all future binders
      abstract class TreeMakerToProp extends (TreeMaker => Prop) {
        // requires(if (!substitutionComputed))
        def updateSubstitution(tm: TreeMaker): Unit = {
          val subst = tm.subPatternsAsSubstitution

          tm match {
            case x @ ExtractorTreeMaker(_, None, binder) =>
              val extractor = accumSubst(normalize(x.extractor))
              extractBinders.collectFirst {
                case (t, reuseBinder) if equivalentTree(t, extractor) => reuseBinder
              } match {
                case Some(reuseBinder) => normalize >>= Substitution(binder, binderToUniqueTree(reuseBinder))
                case None              => extractBinders(extractor) = binder
              }
            case _ =>
          }

          // find part of substitution that replaces bound symbols by new symbols, and reverse that part
          // so that we don't introduce new aliases for existing symbols, thus keeping the set of bound symbols minimal

          // HOT Method for allocation, hence the imperative style here
          val substSize = subst.from.length
          val boundFrom = new mutable.ListBuffer[Tree]()
          val boundTo = new mutable.ListBuffer[Symbol]
          val unboundFrom = new mutable.ArrayBuffer[Symbol](substSize)
          val unboundTo = new mutable.ListBuffer[Tree]
          foreach2(subst.from, subst.to) {
            case (f, t: Ident) if t.symbol.exists && pointsToBound(f) =>
              boundFrom += CODE.REF(f)
              boundTo += t.symbol
            case (f, t) =>
              unboundFrom += f
              unboundTo += normalize(t)
          }
          // reverse substitution that would otherwise replace a variable we already encountered by a new variable
          // NOTE: this forgets the more precise type we have for these later variables, but that's probably okay
          normalize >>= Substitution(boundTo.toList, boundFrom.toList)
          // debug.patmat ("normalize subst: "+ normalize)

          val okSubst = Substitution(unboundFrom.toList, unboundTo.toList) // it's important substitution does not duplicate trees here -- it helps to keep hash consing simple, anyway
          foreach2(okSubst.from, okSubst.to){(f, t) =>
            if (pointsToBound.exists(sym => t.exists(_.symbol == sym)) || tm.isInstanceOf[ExtractorTreeMaker])
              pointsToBound += f
          }
          // debug.patmat("pointsToBound: "+ pointsToBound)

          accumSubst >>= okSubst
          // debug.patmat("accumSubst: "+ accumSubst)
        }

        def handleUnknown(tm: TreeMaker): Prop

        /** apply itself must render a faithful representation of the TreeMaker
         *
         * Concretely, True must only be used to represent a TreeMaker that is sure to match and that does not do any computation at all
         * e.g., doCSE relies on apply itself being sound in this sense (since it drops TreeMakers that are approximated to True -- scala/bug#6077)
         *
         * handleUnknown may be customized by the caller to approximate further
         *
         * TODO: don't ignore outer-checks
         */
        def apply(tm: TreeMaker): Prop = {
          if (!substitutionComputed) updateSubstitution(tm)

          tm match {
            case ttm @ TypeTestTreeMaker(_, _, _, _)                  => ttm.renderCondition(condStrategy)
            case EqualityTestTreeMaker(prevBinder, patTree, _)        => uniqueEqualityProp(binderToUniqueTree(prevBinder), unique(patTree))
            case AlternativesTreeMaker(_, altss, _)                   => \/(altss map (alts => /\(alts map this)))
            case ProductExtractorTreeMaker(testedBinder, None)        => uniqueNonNullProp(binderToUniqueTree(testedBinder))
            case SubstOnlyTreeMaker(_, _)                             => True
            case NonNullTestTreeMaker(prevBinder, _, _)               => uniqueNonNullProp(binderToUniqueTree(prevBinder))
            case GuardTreeMaker(guard) if guard.tpe == ConstantTrue   => True
            case GuardTreeMaker(guard) if guard.tpe == ConstantFalse  => False
            case _                                                    => handleUnknown(tm)
          }
        }
      }

      object condStrategy extends TypeTestTreeMaker.TypeTestCondStrategy {
        type Result                                           = Prop
        def and(a: Result, b: Result)                         = And(a, b)
        def withOuterTest(testedBinder: Symbol, expectedTp: Type) = True // TODO OuterEqProp(testedBinder, expectedType)
        def typeTest(b: Symbol, pt: Type) = { // a type test implies the tested path is non-null (null.isInstanceOf[T] is false for all T)
          val p = binderToUniqueTree(b);                        And(uniqueNonNullProp(p), uniqueTypeProp(p, uniqueTp(pt)))
        }
        def nonNullTest(testedBinder: Symbol)                 = uniqueNonNullProp(binderToUniqueTree(testedBinder))
        def equalsTest(pat: Tree, testedBinder: Symbol)       = uniqueEqualityProp(binderToUniqueTree(testedBinder), unique(pat))
        // rewrite eq test to type test against the singleton type `pat.tpe`; unrelated to == (uniqueEqualityProp), could be null
        def eqTest(pat: Tree, testedBinder: Symbol)           = uniqueTypeProp(binderToUniqueTree(testedBinder), uniqueTp(pat.tpe))
        def tru                                               = True
      }


      private def isIrrefutabilityProof(sym: Symbol): Boolean = {
        sym.isMethod && sym.name == nme.isEmpty && {
          // ConstantFalse is foldable but in joint compilation (bug?) this will be a literal type
          // with case using `==` rather than `=:=` we need to do this instead. neg/t12240.scala
          sym.tpe.finalResultType match {
            case c: ConstantType => c.value == Constant(false)
            case _ => false
          }
        }
      }
        // will an extractor with unapply method of methodtype `tp` always succeed?
        // note: this assumes the other side-conditions implied by the extractor are met
        // (argument of the right type, length check succeeds for unapplySeq,...)
      private def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        //Some(x) is irrefutable
        case TypeRef(_, SomeClass, _) => true
        //name based pattern matching checks for constant false `isEmpty`.
        case TypeRef(_, res, _)       => res.tpe.members.exists(isIrrefutabilityProof)
        //`true.type` is irrefutable for boolean extractors
        case c: ConstantType          => c.value == Constant(true)
        case _                        => false
      }

      private val irrefutableExtractor: PartialFunction[TreeMaker, Prop] = {
        // if the extra condition is None, the extractor's result indicates it always succeeds,
        // (the potential type-test for the argument is represented by a separate TypeTestTreeMaker)
        case ExtractorTreeMaker(extractor, None, _) if irrefutableExtractorType(extractor.tpe) => True
        // Otherwise, if we call the pattern irrefutable here, these conditions
        // are no longer checked and considered true in exhaustiveness and
        // reachability checking.
        // Therefore, the below case alone would treat too much "irrefutable"
        // that really isn't. Something similar is needed (perhaps elsewhere)
        // to check whether a set of unapplySeq's with all arities is toegether
        // exhaustive
        //case p @ ExtractorTreeMaker(extractor, Some(conditions), _) if irrefutableExtractorType(extractor.tpe) => True
      }

      // special-case: interpret pattern `List()` as `Nil`
      // as of 2.13, List.unapply returns an UnapplySeqWrapper (rather than a List)
      // TODO: make it more general List(1, 2) => 1 :: 2 :: Nil  -- not sure this is a good idea...
      private val rewriteListPattern: PartialFunction[TreeMaker, Prop] = {
        case p @ ExtractorTreeMaker(_, _, testedBinder)
          if testedBinder.tpe.typeSymbol == UnapplySeqWrapperClass && p.checkedLength == Some(0) =>
            uniqueEqualityProp(binderToUniqueTree(p.prevBinder), unique(Ident(NilModule) setType NilModule.tpe))
      }
      val fullRewrite      = (irrefutableExtractor orElse rewriteListPattern)
      val refutableRewrite = irrefutableExtractor

      @inline def onUnknown(handler: TreeMaker => Prop) = new TreeMakerToProp {
        def handleUnknown(tm: TreeMaker) = handler(tm)
      }

      // used for CSE -- rewrite all unknowns to False (the most conservative option)
      object conservative extends TreeMakerToProp {
        def handleUnknown(tm: TreeMaker) = False
      }

      final def approximateMatch(cases: List[List[TreeMaker]], treeMakerToProp: TreeMakerToProp = conservative) ={
        val testss = cases.map { _ map (tm => Test(treeMakerToProp(tm), tm)) }
        substitutionComputed = true // a second call to approximateMatch should not re-compute the substitution (would be wrong)
        testss
      }
    }

    def approximateMatchConservative(root: Symbol, cases: List[List[TreeMaker]]): List[List[Test]] =
      (new TreeMakersToProps(root)).approximateMatch(cases)

    // turns a case (represented as a list of abstract tests)
    // into a proposition that is satisfiable if the case may match
    protected final def caseWithoutBodyToProp(tests: List[Test]): Prop =
      /\(tests.takeWhile(t => !t.treeMaker.isInstanceOf[BodyTreeMaker]).map(t => t.prop))

    def showTreeMakers(cases: List[List[TreeMaker]]) = {
      debug.patmat("treeMakers:")
      debug.patmat(alignAcrossRows(cases, ">>"))
    }
  }
}

trait MatchAnalysis extends MatchApproximation {
  import global._
  import global.definitions._

  trait MatchAnalyzer extends MatchApproximator  {
    def uncheckedWarning(pos: Position, msg: String, site: Symbol) = runReporting.warning(pos, msg, WarningCategory.Unchecked, site)
    def warn(pos: Position, ex: AnalysisBudget.Exception, kind: String, site: Symbol) = uncheckedWarning(pos, s"Cannot check match for $kind.\n${ex.advice}", site)
    def reportWarning(message: String) = typer.context.warning(typer.context.tree.pos, message, WarningCategory.OtherMatchAnalysis)

  // TODO: model dependencies between variables: if V1 corresponds to (x: List[_]) and V2 is (x.hd), V2 cannot be assigned when V1 = null or V1 = Nil
    // right now hackily implement this by pruning counter-examples
    // unreachability would also benefit from a more faithful representation


    // reachability (dead code)

    // computes the first 0-based case index that is unreachable (if any)
    // a case is unreachable if it implies its preceding cases
    // call C the formula that is satisfiable if the considered case matches
    // call P the formula that is satisfiable if the cases preceding it match
    // the case is reachable if there is a model for -P /\ C,
    // thus, the case is unreachable if there is no model for -(-P /\ C),
    // or, equivalently, P \/ -C, or C => P
    def unreachableCase(prevBinder: Symbol, cases: List[List[TreeMaker]], @unused pt: Type): Option[Int] = {
      debug.patmat("reachability analysis")
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.patmatAnaReach) else null

      // use the same approximator so we share variables,
      // but need different conditions depending on whether we're conservatively looking for failure or success
      // don't rewrite List-like patterns, as List() and Nil need to be distinguished for unreachability
      val approx = new TreeMakersToProps(prevBinder)
      def approximate(default: Prop) = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.refutableRewrite.applyOrElse(tm, (_: TreeMaker) => default )
      })

      val propsCasesOk   = approximate(True)  map caseWithoutBodyToProp
      val propsCasesFail = approximate(False) map (t => Not(caseWithoutBodyToProp(t)))

      try {
        val (eqAxiomsFail, symbolicCasesFail) = removeVarEq(propsCasesFail, modelNull = true)
        val (eqAxiomsOk, symbolicCasesOk) = removeVarEq(propsCasesOk, modelNull = true)
        val eqAxioms = simplify(And(eqAxiomsOk, eqAxiomsFail)) // I'm pretty sure eqAxiomsOk == eqAxiomsFail, but not 100% sure.

        val prefix = mutable.ArrayBuffer[Prop]()
        prefix += eqAxioms

        var prefixRest = symbolicCasesFail
        var current = symbolicCasesOk
        var reachable = true
        var caseIndex = 0

        debug.patmat("reachability, vars:\n" + ((propsCasesFail flatMap gatherVariables).distinct map (_.describe) mkString ("\n")))
        debug.patmat(s"equality axioms:\n$eqAxiomsOk")

        // invariant (prefixRest.length == current.length) && (prefix.reverse ++ prefixRest == symbolicCasesFail)
        // termination: prefixRest.length decreases by 1
        while (prefixRest.nonEmpty && reachable) {
          val prefHead = prefixRest.head
          caseIndex += 1
          prefixRest = prefixRest.tail
          if (prefixRest.isEmpty) reachable = true
          else {
            prefix += prefHead
            current = current.tail
            val and = And((current.head +: prefix).toIndexedSeq: _*)
            reachable = hasModel(eqFreePropToSolvable(and))
          }
        }

        if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.patmatAnaReach, start)

        if (reachable) None else Some(caseIndex)
      } catch {
        case ex: AnalysisBudget.Exception =>
          warn(prevBinder.pos, ex, "unreachability", prevBinder)
          None // CNF budget exceeded
      }
    }

    // exhaustivity

    def exhaustive(prevBinder: Symbol, cases: List[List[TreeMaker]], @unused pt: Type): List[String] = if (!settings.warnStrictUnsealedPatMat && uncheckableType(prevBinder.info)) Nil else {
      debug.patmat("exhaustiveness analysis")
      // customize TreeMakersToProps (which turns a tree of tree makers into a more abstract DAG of tests)
      // - approximate the pattern `List()` (unapplySeq on List with empty length) as `Nil`,
      //   otherwise the common (xs: List[Any]) match { case List() => case x :: xs => } is deemed unexhaustive
      // - back off (to avoid crying exhaustive too often) in unhandled cases
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.patmatAnaExhaust) else null
      var backoff = false
      val strict = !settings.nonStrictPatmatAnalysis.value

      val approx = new TreeMakersToProps(prevBinder)
      val symbolicCases = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.fullRewrite.applyOrElse[TreeMaker, Prop](tm, {
          case BodyTreeMaker(_, _) => True // irrelevant -- will be discarded by symbolCase later
          case ExtractorTreeMaker(_, _, _)
             | ProductExtractorTreeMaker(_, _)
             | GuardTreeMaker(_) if strict =>
            False
          case _ =>
            debug.patmat("backing off due to "+ tm)
            backoff = true
            False
        })
      }) map caseWithoutBodyToProp

      if (backoff) Nil else {
        val prevBinderTree = approx.binderToUniqueTree(prevBinder)

        // TODO: null tests generate too much noise, so disabled them -- is there any way to bring them back?
        // assuming we're matching on a non-null scrutinee (prevBinder), when does the match fail?
        // val nonNullScrutineeCond =
        //   assume non-null for all the components of the tuple we're matching on (if we're matching on a tuple)
        //   if (isTupleType(prevBinder.tpe))
        //     prevBinder.tpe.typeArgs.mapWithIndex{case (_, i) => NonNullProp(codegen.tupleSel(prevBinderTree)(i))}.reduceLeft(And)
        //   else
        //     NonNullProp(prevBinderTree)
        // val matchFails = And(symbolic(nonNullScrutineeCond), Not(symbolicCases reduceLeft (Or(_, _))))

        // when does the match fail?
        val matchFails = Not(\/(symbolicCases))

        // debug output:
        debug.patmat("analysing:")
        showTreeMakers(cases)

        // debug.patmat("\nvars:\n"+ (vars map (_.describe) mkString ("\n")))
        // debug.patmat("\nmatchFails as CNF:\n"+ cnfString(propToSolvable(matchFails)))

        try {
          // find the models (under which the match fails)
          val matchFailModels = findAllModelsFor(propToSolvable(matchFails), prevBinder)

          val scrutVar = Var(prevBinderTree)
          val counterExamples = matchFailModels.iterator.flatMap { model =>
            expandModel(model).flatMap(modelToCounterExample(scrutVar))
          }.take(AnalysisBudget.maxDPLLdepth).toList

          // sorting before pruning is important here in order to
          // keep neg/t7020.scala stable
          // since e.g. List(_, _) would cover List(1, _)
          // and make sure the strings are distinct, see Shmeez & TestSequence06 in run/patmatnew.scala
          val pruned = CounterExample.prune(counterExamples.sortBy(_.toString)).map(_.toString).distinct

          if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.patmatAnaExhaust, start)
          pruned
        } catch {
          case ex: AnalysisBudget.Exception =>
            warn(prevBinder.pos, ex, "exhaustivity", prevBinder)
            Nil // CNF budget exceeded
        }
      }
    }

    object CounterExample {
      def prune(examples: List[CounterExample]): List[CounterExample] = {
        // scala/bug#7669 Warning: we don't used examples.distinct here any more as
        //         we can have A != B && A.coveredBy(B) && B.coveredBy(A)
        //         with Nil and List().
        val result = mutable.Buffer[CounterExample]()
        for (example <- examples if (!result.exists(example coveredBy _)))
          result += example
        result.toList
      }
    }

    // a way to construct a value that will make the match fail: a constructor invocation, a constant, an object of some type)
    class CounterExample {
      protected[MatchAnalyzer] def flattenConsArgs: List[CounterExample] = Nil
      def coveredBy(other: CounterExample): Boolean = this == other || other == WildcardExample
    }
    case class ValueExample(c: ValueConst) extends CounterExample { override def toString = c.toString }
    case class TypeExample(c: Const)  extends CounterExample { override def toString = "(_ : "+ c +")" }
    case class NegativeExample(eqTo: Const, nonTrivialNonEqualTo: List[Const]) extends CounterExample {
      // require(nonTrivialNonEqualTo.nonEmpty, nonTrivialNonEqualTo)
      override def toString = {
        val negation =
          if (nonTrivialNonEqualTo.tail.isEmpty) nonTrivialNonEqualTo.head.toString
          else nonTrivialNonEqualTo.map(_.toString).sorted.mkString("(", ", ", ")")
        "(x: "+ eqTo +" forSome x not in "+ negation +")"
      }
    }
    case class ListExample(ctorArgs: List[CounterExample]) extends CounterExample {
      protected[MatchAnalyzer] override def flattenConsArgs: List[CounterExample] = ctorArgs match {
        case hd :: tl :: Nil => hd :: tl.flattenConsArgs
        case _ => Nil
      }
      protected[MatchAnalyzer] lazy val elems = flattenConsArgs

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case other@ListExample(_) =>
            this == other || ((elems.sizeCompare(other.elems) == 0) && (elems zip other.elems).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }

      override def toString = elems.mkString("List(", ", ", ")")
    }
    case class TupleExample(ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = ctorArgs.mkString("(", ", ", ")")

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case TupleExample(otherArgs) =>
            this == other || ((ctorArgs.sizeCompare(otherArgs) == 0) && (ctorArgs zip otherArgs).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }
    }
    case class ConstructorExample(cls: Symbol, ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = cls.decodedName + (if (cls.isModuleClass) "" else ctorArgs.mkString("(", ", ", ")"))
    }

    case object WildcardExample extends CounterExample { override def toString = "_" }
    case object NoExample extends CounterExample { override def toString = "??" }

    type VarAssignment = Map[Var, (Seq[Const], Seq[Const])]

    // returns a mapping from variable to
    // equal and notEqual symbols
    def modelToVarAssignment(model: Model): VarAssignment =
      model.toSeq.groupBy(_._1.variable).view.mapValues{ xs =>
        val (trues, falses) = xs.partition(_._2)
        (trues map (_._1.const), falses map (_._1.const))
        // should never be more than one value in trues...
      }.to(Map)

    def varAssignmentString(varAssignment: VarAssignment) =
      varAssignment.toSeq.sortBy(_._1.toString).map { case (v, (trues, falses)) =>
        s"$v(=${v.path}: ${v.staticTpCheckable}) == ${trues.mkString("(", ", ", ")")}  != (${falses.mkString(", ")})"
      }.mkString("\n")

    /**
     * The models we get from the DPLL solver need to be mapped back to counter examples.
     * However there's no precalculated mapping model -> counter example. Even worse,
     * not every valid model corresponds to a valid counter example.
     * The reason is that restricting the valid models further would for example require
     * a quadratic number of additional clauses. So to keep the optimistic case fast
     * (i.e., all cases are covered in a pattern match), the infeasible counter examples
     * are filtered later.
     *
     * The DPLL procedure keeps the literals that do not contribute to the solution
     * unassigned, e.g., for  `(a \/ b)`
     * only {a = true} or {b = true} is required and the other variable can have any value.
     *
     * This function does a smart expansion of the model and avoids models that
     * have conflicting mappings.
     *
     * For example for in case of the given set of symbols (taken from `t7020.scala`):
     *  "V2=2#16"
     *  "V2=6#19"
     *  "V2=5#18"
     *  "V2=4#17"
     *  "V2=7#20"
     *
     * One possibility would be to group the symbols by domain but
     * this would only work for equality tests and would not be compatible
     * with type tests.
     * Another observation leads to a much simpler algorithm:
     * Only one of these symbols can be set to true,
     * since `V2` can at most be equal to one of {2,6,5,4,7}.
     */
    def expandModel(solution: Solution): List[VarAssignment] = {

      val model = solution.model

      // x1 = ...
      // x1.hd = ...
      // x1.tl = ...
      // x1.hd.hd = ...
      // ...
      val varAssignment = modelToVarAssignment(model)
      debug.patmat("var assignment for model " + model + ":\n" + varAssignmentString(varAssignment))

      // group symbols that assign values to the same variables (i.e., symbols are mutually exclusive)
      // (thus the groups are sets of disjoint assignments to variables)
      val groupedByVar: Map[Var, List[Sym]] = solution.unassigned.groupBy(_.variable)

      val expanded = for {
        (variable, syms) <- groupedByVar.toList.sortBy(_._1.toString)
      } yield {

        val (equal, notEqual) = varAssignment.getOrElse(variable, Nil -> Nil)

        def addVarAssignment(equalTo: List[Const], notEqualTo: List[Const]) =
          Map(variable ->((equal ++ equalTo, notEqual ++ notEqualTo)))

        // this assignment is needed in case that
        // there exists already an assign
        val allNotEqual = addVarAssignment(Nil, syms.map(_.const))

        // this assignment is conflicting on purpose:
        // a list counter example could contain wildcards: e.g. `List(_,_)`
        val allEqual = addVarAssignment(syms.map(_.const), Nil)

        if (equal.isEmpty) {
          val oneHot = for {
            s <- syms
          } yield {
            addVarAssignment(List(s.const), syms.filterNot(_ == s).map(_.const))
          }
          allEqual :: allNotEqual :: oneHot
        } else {
          allEqual :: allNotEqual :: Nil
        }
      }

      // we need the Cartesian product here,
      // since we want to report all missing cases
      // (i.e., combinations)
      @tailrec def loop(acc: List[VarAssignment], in: List[List[VarAssignment]]): List[VarAssignment] = {
        if (acc.sizeIs > AnalysisBudget.maxDPLLdepth) acc.take(AnalysisBudget.maxDPLLdepth)
        else in match {
          case vs :: vss => loop(for (map1 <- acc; map2 <- vs) yield map1 ++ map2, vss)
          case _         => acc
        }
      }
      expanded match {
        case head :: tail =>
          val cartesianProd = loop(head, tail)
          // add expanded variables
          // note that we can just use `++`
          // since the Maps have disjoint keySets
          for (m <- cartesianProd) yield varAssignment ++ m
        case _            => List(varAssignment)
      }
    }

    // return constructor call when the model is a true counter example
    // (the variables don't take into account type information derived from other variables,
    //  so, naively, you might try to construct a counter example like _ :: Nil(_ :: _, _ :: _),
    //  since we didn't realize the tail of the outer cons was a Nil)
    def modelToCounterExample(scrutVar: Var)(varAssignment: VarAssignment): Option[CounterExample] = {
      val strict = !settings.nonStrictPatmatAnalysis.value

      // chop a path into a list of symbols
      def chop(path: Tree): List[Symbol] = path match {
        case Ident(_) => List(path.symbol)
        case Select(pre, name) => chop(pre) :+ path.symbol
        case Apply(fun, args) => chop(fun) :+ path.symbol
        case _ =>
          // debug.patmat("don't know how to chop "+ path)
          Nil
      }

      // turn the variable assignments into a tree
      // the root is the scrutinee (x1), edges are labelled by the fields that are assigned
      // a node is a variable example (which is later turned into a counter example)
      object VariableAssignment {
        private def findVar(path: List[Symbol]) = path match {
          case List(root) if root == scrutVar.path.symbol => Some(scrutVar)
          case _ => varAssignment.find{case (v, _) => chop(v.path) == path}.map(_._1)
        }

        private val uniques = new mutable.HashMap[Var, VariableAssignment]
        private def unique(variable: Var): VariableAssignment =
          uniques.getOrElseUpdate(variable, {
            val (eqTo, neqTo) = varAssignment.getOrElse(variable, (Nil, Nil)) // TODO
            VariableAssignment(variable, eqTo.toList, neqTo.toList)
          })

        def apply(variable: Var): VariableAssignment = {
          val path  = chop(variable.path)
          val pre   = path.init
          val field = path.last

          val newCtor = unique(variable)

          if (pre.isEmpty) newCtor
          else {
            findVar(pre) foreach { preVar =>
              val outerCtor = this(preVar)
              outerCtor.addField(field, newCtor)
            }
            newCtor
          }
        }
      }

      // node in the tree that describes how to construct a counter-example
      case class VariableAssignment(variable: Var, equalTo: List[Const], notEqualTo: List[Const]) {
        private val fields: mutable.LinkedHashMap[Symbol, VariableAssignment] = mutable.LinkedHashMap.empty
        // need to prune since the model now incorporates all super types of a constant (needed for reachability)
        private lazy val uniqueEqualTo = equalTo filterNot (subsumed => equalTo.exists(better => (better ne subsumed) && instanceOfTpImplies(better.tp, subsumed.tp)))
        private lazy val inSameDomain = uniqueEqualTo forall (const => variable.domainSyms.exists(_.exists(_.const.tp =:= const.tp)))
        private lazy val prunedEqualTo = uniqueEqualTo filterNot (subsumed => variable.staticTpCheckable <:< subsumed.tp)
        private lazy val ctor       = (prunedEqualTo match { case List(TypeConst(tp)) => tp case _ => variable.staticTpCheckable }).typeSymbol.primaryConstructor
        private lazy val ctorParams = if (ctor.paramss.isEmpty) Nil else ctor.paramss.head
        private lazy val cls        = ctor.safeOwner
        private lazy val caseFieldAccs = cls.caseFieldAccessors

        def addField(symbol: Symbol, assign: VariableAssignment): Unit = {
          // scala/bug#7669 Only register this field if if this class contains it.
          val shouldConstrainField = !symbol.isCaseAccessor || caseFieldAccs.contains(symbol)
          if (shouldConstrainField) fields(symbol) = assign
        }

        def allFieldAssignmentsLegal: Boolean =
          (fields.keySet subsetOf caseFieldAccs.toSet) && fields.values.forall(_.allFieldAssignmentsLegal)

        private lazy val nonTrivialNonEqualTo = notEqualTo.filterNot{c => c.isAny }

        // NoExample if the constructor call is ill-typed
        // (thus statically impossible -- can we incorporate this into the formula?)
        // beBrief is used to suppress negative information nested in tuples -- it tends to get too noisy
        def toCounterExample(beBrief: Boolean = false): Option[CounterExample] =
          if (!allFieldAssignmentsLegal) Some(NoExample)
          else {
            debug.patmat("describing "+ ((variable, equalTo, notEqualTo, fields, cls, allFieldAssignmentsLegal)))
            val res = prunedEqualTo match {
              // a definite assignment to a value
              case List(eq: ValueConst) if fields.isEmpty => Some(ValueExample(eq))

              // constructor call
              // or we did not gather any information about equality but we have information about the fields
              //  --> typical example is when the scrutinee is a tuple and all the cases first unwrap that tuple and only then test something interesting
              case _ if cls != NoSymbol && !isPrimitiveValueClass(cls) &&
                        (  uniqueEqualTo.nonEmpty
                        || (fields.nonEmpty && prunedEqualTo.isEmpty && notEqualTo.isEmpty)) =>

                def args(brevity: Boolean = beBrief) = {
                  // figure out the constructor arguments from the field assignment
                  val argLen = (caseFieldAccs.length min ctorParams.length)

                  val examples = (0 until argLen).map(i => fields.get(caseFieldAccs(i)).map(_.toCounterExample(brevity)) getOrElse Some(WildcardExample)).toList
                  sequenceOpt(examples)
                }

                cls match {
                  case ConsClass                                =>
                    args().map {
                      case List(NoExample, l: ListExample) =>
                        // special case for neg/t7020.scala:
                        // if we find a counter example `??::*` we report `*::*` instead
                        // since the `??` originates from uniqueEqualTo containing several instanced of the same type
                        List(WildcardExample, l)
                      case args                            => args
                    }.map(ListExample)
                  case _ if isTupleSymbol(cls)                  => args(brevity = true).map(TupleExample)
                  case _ if cls.isSealed && (cls.isAbstractClass || cls.hasJavaEnumFlag) =>
                    // don't report sealed abstract classes, since
                    // 1) they can't be instantiated
                    // 2) we are already reporting any missing subclass (since we know the full domain)
                    // (see patmatexhaust.scala)
                    None
                  case _                                        => args().map(ConstructorExample(cls, _))
                }

              // a definite assignment to a type
              case List(eq) if fields.isEmpty => Some(TypeExample(eq))

              // negative information
              case Nil if nonTrivialNonEqualTo.nonEmpty =>
                // negation tends to get pretty verbose
                if (beBrief) Some(WildcardExample)
                else {
                  val eqTo = equalTo.headOption getOrElse TypeConst(variable.staticTpCheckable)
                  Some(NegativeExample(eqTo, nonTrivialNonEqualTo))
                }

              // if uniqueEqualTo contains more than one symbol of the same domain
              // then we can safely ignore these counter examples since we will eventually encounter
              // both counter examples separately
              // ... in strict mode, consider variable assignment as a wild counter-example
              case _ if inSameDomain => if (strict) Some(WildcardExample) else None

              // not a valid counter-example, possibly since we have a definite type but there was a field mismatch
              // TODO: improve reasoning -- in the mean time, a false negative is better than an annoying false positive
              case _ => Some(NoExample)
            }
            debug.patmatResult("described as")(res)
          }

        override def toString = toCounterExample().toString
      }

      // slurp in information from other variables
      varAssignment.keys.toSeq.sortBy(_.toString).foreach(v => if (v != scrutVar) VariableAssignment(v))

      // this is the variable we want a counter example for
      VariableAssignment(scrutVar).toCounterExample()
    }

    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit = {
      if (!suppression.suppressUnreachable) {
        unreachableCase(prevBinder, cases, pt) foreach { caseIndex =>
          reportUnreachable(cases(caseIndex).last.pos)
        }
      }
      if (!suppression.suppressExhaustive) {
        val counterExamples = exhaustive(prevBinder, cases, pt)
        if (counterExamples.nonEmpty)
          reportMissingCases(prevBinder.pos, counterExamples)
      }
    }
  }
}
