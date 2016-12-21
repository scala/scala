/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.symtab.Flags.{SYNTHETIC, ARTIFACT}
import scala.language.postfixOps
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.util.Position

/** Translate our IR (TreeMakers) into actual Scala Trees using the factory methods in MatchCodeGen.
 *
 * The IR is mostly concerned with sequencing, substitution, and rendering all necessary conditions,
 * mostly agnostic to whether we're in optimized/pure (virtualized) mode.
 */
trait MatchTreeMaking extends MatchCodeGen with Debugging {
  import global._
  import definitions._

  final case class Suppression(suppressExhaustive: Boolean, suppressUnreachable: Boolean)
  object Suppression {
    val NoSuppression = Suppression(false, false)
    val FullSuppression = Suppression(true, true)
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// the making of the trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TreeMakers extends TypedSubstitution with CodegenCore {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): (List[List[TreeMaker]], List[Tree])
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree], unchecked: Boolean): Option[Tree] =
      None

    // for catch (no need to customize match failure)
    def emitTypeSwitch(bindersAndCases: List[(Symbol, List[TreeMaker])], pt: Type): Option[List[CaseDef]] =
      None

    abstract class TreeMaker {
      def pos: Position

      /** captures the scope and the value of the bindings in patterns
       * important *when* the substitution happens (can't accumulate and do at once after the full matcher has been constructed)
       */
      def substitution: Substitution =
        if (currSub eq null) localSubstitution
        else currSub

      protected def localSubstitution: Substitution

      private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        if (currSub ne null) {
          debug.patmat("BUG: incorporateOuterSubstitution called more than once for "+ ((this, currSub, outerSubst)))
          Thread.dumpStack()
        }
        else currSub = outerSubst >> substitution
      }
      private[this] var currSub: Substitution = null

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
       *
       * Should not be used to perform actual substitution!
       * Only used to reason symbolically about the values the subpattern binders are bound to.
       * See TreeMakerToCond#updateSubstitution.
       *
       * Overridden in PreserveSubPatBinders to pretend it replaces the subpattern binders by subpattern refs
       * (Even though we don't do so anymore -- see SI-5158, SI-5739 and SI-6070.)
       *
       * TODO: clean this up, would be nicer to have some higher-level way to compute
       * the binders bound by this tree maker and the symbolic values that correspond to them
       */
      def subPatternsAsSubstitution: Substitution = substitution

      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree)(casegen: Casegen): Tree
    }

    sealed trait NoNewBinders extends TreeMaker {
      protected val localSubstitution: Substitution = EmptySubstitution
    }

    case class TrivialTreeMaker(tree: Tree) extends TreeMaker with NoNewBinders {
      def pos = tree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = tree
    }

    case class BodyTreeMaker(body: Tree, matchPt: Type) extends TreeMaker with NoNewBinders {
      def pos = body.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = // assert(next eq EmptyTree)
        atPos(body.pos)(casegen.one(substitution(body))) // since SubstOnly treemakers are dropped, need to do it here
      override def toString = "B"+((body, matchPt))
    }

    case class SubstOnlyTreeMaker(prevBinder: Symbol, nextBinder: Symbol) extends TreeMaker {
      val pos = NoPosition

      val localSubstitution = Substitution(prevBinder, CODE.REF(nextBinder))
      def chainBefore(next: Tree)(casegen: Casegen): Tree = substitution(next)
      override def toString = "S"+ localSubstitution
    }

    sealed abstract class FunTreeMaker extends TreeMaker {
      val nextBinder: Symbol
      def pos = nextBinder.pos
    }

    sealed abstract class CondTreeMaker extends FunTreeMaker {
      val prevBinder: Symbol
      val nextBinderTp: Type
      val cond: Tree
      val res: Tree

      lazy val nextBinder = freshSym(pos, nextBinderTp)
      lazy val localSubstitution = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))

      def chainBefore(next: Tree)(casegen: Casegen): Tree =
        atPos(pos)(casegen.flatMapCond(cond, res, nextBinder, substitution(next)))
    }

    // unless we're optimizing, emit local variable bindings for all subpatterns of extractor/case class patterns
    protected val debugInfoEmitVars = !settings.optimise.value

    sealed trait PreserveSubPatBinders extends TreeMaker {
      val subPatBinders: List[Symbol]
      val subPatRefs: List[Tree]
      val ignoredSubPatBinders: Set[Symbol]

      // unless `debugInfoEmitVars`, this set should contain the bare minimum for correctness
      // mutable case class fields need to be stored regardless (SI-5158, SI-6070) -- see override in ProductExtractorTreeMaker
      // sub patterns bound to wildcard (_) are never stored as they can't be referenced
      // dirty debuggers will have to get dirty to see the wildcards
      lazy val storedBinders: Set[Symbol] =
        (if (debugInfoEmitVars) subPatBinders.toSet else Set.empty) ++ extraStoredBinders -- ignoredSubPatBinders

      // e.g., mutable fields of a case class in ProductExtractorTreeMaker
      def extraStoredBinders: Set[Symbol]

      def emitVars = storedBinders.nonEmpty

      private lazy val (stored, substed) = (subPatBinders, subPatRefs).zipped.partition{ case (sym, _) => storedBinders(sym) }

      protected lazy val localSubstitution: Substitution = if (!emitVars) Substitution(subPatBinders, subPatRefs)
        else {
          val (subPatBindersSubstituted, subPatRefsSubstituted) = substed.unzip
          Substitution(subPatBindersSubstituted.toList, subPatRefsSubstituted.toList)
        }

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
       *
       * We pretend to replace the subpattern binders by subpattern refs
       * (Even though we don't do so anymore -- see SI-5158, SI-5739 and SI-6070.)
       */
      override def subPatternsAsSubstitution =
        Substitution(subPatBinders, subPatRefs) >> super.subPatternsAsSubstitution

      def bindSubPats(in: Tree): Tree =
        if (!emitVars) in
        else {
          // binders in `subPatBindersStored` that are referenced by tree `in`
          val usedBinders = new mutable.HashSet[Symbol]()
          // all potentially stored subpat binders
          val potentiallyStoredBinders = stored.unzip._1.toSet
          def ref(sym: Symbol) =
            if (potentiallyStoredBinders(sym)) usedBinders += sym
          // compute intersection of all symbols in the tree `in` and all potentially stored subpat binders
          in.foreach {
            case tt: TypeTree =>
              tt.tpe foreach { // SI-7459 e.g. case Prod(t) => new t.u.Foo
                case SingleType(_, sym) => ref(sym)
                case _ =>
              }
            case t => ref(t.symbol)
          }

          if (usedBinders.isEmpty) in
          else {
            // only store binders actually used
            val (subPatBindersStored, subPatRefsStored) = stored.filter{case (b, _) => usedBinders(b)}.unzip
            Block(map2(subPatBindersStored.toList, subPatRefsStored.toList)(ValDef(_, _)), in)
          }
        }
    }

    /**
     * Make a TreeMaker that will result in an extractor call specified by `extractor`
     * the next TreeMaker (here, we don't know which it'll be) is chained after this one by flatMap'ing
     * a function with binder `nextBinder` over our extractor's result
     * the function's body is determined by the next TreeMaker
     * (furthermore, the interpretation of `flatMap` depends on the codegen instance we're using).
     *
     * The values for the subpatterns, as computed by the extractor call in `extractor`,
     * are stored in local variables that re-use the symbols in `subPatBinders`.
     * This makes extractor patterns more debuggable (SI-5739).
     */
    case class ExtractorTreeMaker(extractor: Tree, extraCond: Option[Tree], nextBinder: Symbol)(
          val subPatBinders: List[Symbol],
          val subPatRefs: List[Tree],
          val potentiallyMutableBinders: Set[Symbol],
          extractorReturnsBoolean: Boolean,
          val checkedLength: Option[Int],
          val prevBinder: Symbol,
          val ignoredSubPatBinders: Set[Symbol]
          ) extends FunTreeMaker with PreserveSubPatBinders {

      def extraStoredBinders: Set[Symbol] = potentiallyMutableBinders

      debug.patmat(s"""
        |ExtractorTreeMaker($extractor, $extraCond, $nextBinder) {
        |  $subPatBinders
        |  $subPatRefs
        |  $extractorReturnsBoolean
        |  $checkedLength
        |  $prevBinder
        |  $ignoredSubPatBinders
        |}""".stripMargin)

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val condAndNext = extraCond match {
          case Some(cond) =>
            casegen.ifThenElseZero(substitution(cond), bindSubPats(substitution(next)))
          case _ =>
            bindSubPats(substitution(next))
        }
        atPos(extractor.pos)(
          if (extractorReturnsBoolean) casegen.flatMapCond(extractor, CODE.UNIT, nextBinder, condAndNext)
          else casegen.flatMap(extractor, nextBinder, condAndNext)
        )
      }

      override def toString = "X"+((extractor, nextBinder.name))
    }

    /**
     * An optimized version of ExtractorTreeMaker for Products.
     * For now, this is hard-coded to case classes, and we simply extract the case class fields.
     *
     * The values for the subpatterns, as specified by the case class fields at the time of extraction,
     * are stored in local variables that re-use the symbols in `subPatBinders`.
     * This makes extractor patterns more debuggable (SI-5739) as well as
     * avoiding mutation after the pattern has been matched (SI-5158, SI-6070)
     *
     * TODO: make this user-definable as follows
     *   When a companion object defines a method `def unapply_1(x: T): U_1`, but no `def unapply` or `def unapplySeq`,
     *   the extractor is considered to match any non-null value of type T
     *   the pattern is expected to have as many sub-patterns as there are `def unapply_I(x: T): U_I` methods,
     *   and the type of the I'th sub-pattern is `U_I`.
     *   The same exception for Seq patterns applies: if the last extractor is of type `Seq[U_N]`,
     *   the pattern must have at least N arguments (exactly N if the last argument is annotated with `: _*`).
     *   The arguments starting at N (and beyond) are taken from the sequence returned by apply_N,
     *   and it is checked that that sequence has enough elements to provide values for all expected sub-patterns.
     *
     *   For a case class C, the implementation is assumed to be `def unapply_I(x: C) = x._I`,
     *   and the extractor call is inlined under that assumption.
     */
    case class ProductExtractorTreeMaker(prevBinder: Symbol, extraCond: Option[Tree])(
          val subPatBinders: List[Symbol],
          val subPatRefs: List[Tree],
          val mutableBinders: List[Symbol],
          binderKnownNonNull: Boolean,
          val ignoredSubPatBinders: Set[Symbol]
         ) extends FunTreeMaker with PreserveSubPatBinders {

      import CODE._
      val nextBinder = prevBinder // just passing through

      // mutable binders must be stored to avoid unsoundness or seeing mutation of fields after matching (SI-5158, SI-6070)
      def extraStoredBinders: Set[Symbol] = mutableBinders.toSet

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val nullCheck = REF(prevBinder) OBJ_NE NULL
        val cond =
          if (binderKnownNonNull) extraCond
          else (extraCond map (nullCheck AND _)
          orElse Some(nullCheck))

        cond match {
          case Some(cond) =>
            casegen.ifThenElseZero(cond, bindSubPats(substitution(next)))
          case _ =>
            bindSubPats(substitution(next))
        }
      }

      override def toString = "P"+((prevBinder.name,  extraCond getOrElse "", localSubstitution))
    }

    object IrrefutableExtractorTreeMaker {
      // will an extractor with unapply method of methodtype `tp` always succeed?
      // note: this assumes the other side-conditions implied by the extractor are met
      // (argument of the right type, length check succeeds for unapplySeq,...)
      def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        case TypeRef(_, SomeClass, _) => true
        // probably not useful since this type won't be inferred nor can it be written down (yet)
        case ConstantTrue => true
        case _            => false
      }

      def unapply(xtm: ExtractorTreeMaker): Option[(Tree, Symbol)] = xtm match {
        case ExtractorTreeMaker(extractor, None, nextBinder) if irrefutableExtractorType(extractor.tpe) =>
          Some((extractor, nextBinder))
        case _ =>
          None
      }
    }

    object TypeTestTreeMaker {
      // factored out so that we can consistently generate other representations of the tree that implements the test
      // (e.g. propositions for exhaustivity and friends, boolean for isPureTypeTest)
      trait TypeTestCondStrategy {
        type Result

        def outerTest(testedBinder: Symbol, expectedTp: Type): Result
        // TODO: can probably always widen
        def typeTest(testedBinder: Symbol, expectedTp: Type): Result
        def nonNullTest(testedBinder: Symbol): Result
        def equalsTest(pat: Tree, testedBinder: Symbol): Result
        def eqTest(pat: Tree, testedBinder: Symbol): Result
        def and(a: Result, b: Result): Result
        def tru: Result
      }

      object treeCondStrategy extends TypeTestCondStrategy { import CODE._
        type Result = Tree

        def and(a: Result, b: Result): Result                = a AND b
        def tru                                              = mkTRUE
        def typeTest(testedBinder: Symbol, expectedTp: Type) = codegen._isInstanceOf(testedBinder, expectedTp)
        def nonNullTest(testedBinder: Symbol)                = REF(testedBinder) OBJ_NE NULL
        def equalsTest(pat: Tree, testedBinder: Symbol)      = codegen._equals(pat, testedBinder)
        def eqTest(pat: Tree, testedBinder: Symbol)          = REF(testedBinder) OBJ_EQ pat

        def outerTest(testedBinder: Symbol, expectedTp: Type): Tree = {
          val expectedPrefix = expectedTp.prefix
          if (expectedPrefix eq NoType) mkTRUE // fallback for SI-6183
          else {
            // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
            // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
            val outerFor = expectedTp.typeSymbol
            val outerMarker = outerFor.newMethod(vpmName.outer, newFlags = SYNTHETIC | ARTIFACT) setInfo expectedPrefix
            Select(codegen._asInstanceOf(testedBinder, expectedTp), outerMarker) OBJ_EQ gen.mkAttributedQualifier(expectedPrefix)
          }
        }
      }

      object pureTypeTestChecker extends TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = true

        def outerTest(testedBinder: Symbol, expectedTp: Type): Result = false
        def nonNullTest(testedBinder: Symbol): Result                 = false
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false
        def and(a: Result, b: Result): Result                         = false // we don't and type tests, so the conjunction must include at least one false
        def tru                                                       = true
      }

      def nonNullImpliedByTestChecker(binder: Symbol) = new TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = testedBinder eq binder
        def outerTest(testedBinder: Symbol, expectedTp: Type): Result = false
        def nonNullTest(testedBinder: Symbol): Result                 = testedBinder eq binder
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false // could in principle analyse pat and see if it's statically known to be non-null
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false // could in principle analyse pat and see if it's statically known to be non-null
        def and(a: Result, b: Result): Result                         = a || b
        def tru                                                       = false
      }
    }

    /** implements the run-time aspects of (§8.2) (typedPattern has already done the necessary type transformations)
     *
     * Type patterns consist of types, type variables, and wildcards. A type pattern T is of one of the following forms:
        - A reference to a class C, p.C, or T#C.
          This type pattern matches any non-null instance of the given class.
          Note that the prefix of the class, if it is given, is relevant for determining class instances.
          For instance, the pattern p.C matches only instances of classes C which were created with the path p as prefix.
          The bottom types scala.Nothing and scala.Null cannot be used as type patterns, because they would match nothing in any case.

        - A singleton type p.type.
          This type pattern matches only the value denoted by the path p
          (that is, a pattern match involved a comparison of the matched value with p using method eq in class AnyRef). // TODO: the actual pattern matcher uses ==, so that's what I'm using for now
          // https://issues.scala-lang.org/browse/SI-4577 "pattern matcher, still disappointing us at equality time"

        - A compound type pattern T1 with ... with Tn where each Ti is a type pat- tern.
          This type pattern matches all values that are matched by each of the type patterns Ti.

        - A parameterized type pattern T[a1,...,an], where the ai are type variable patterns or wildcards _.
          This type pattern matches all values which match T for some arbitrary instantiation of the type variables and wildcards.
          The bounds or alias type of these type variable are determined as described in (§8.3).

        - A parameterized type pattern scala.Array[T1], where T1 is a type pattern. // TODO
          This type pattern matches any non-null instance of type scala.Array[U1], where U1 is a type matched by T1.
    **/
    case class TypeTestTreeMaker(prevBinder: Symbol, testedBinder: Symbol, expectedTp: Type, nextBinderTp: Type)(override val pos: Position, extractorArgTypeTest: Boolean = false) extends CondTreeMaker {
      import TypeTestTreeMaker._
      debug.patmat("TTTM"+((prevBinder, extractorArgTypeTest, testedBinder, expectedTp, nextBinderTp)))

      lazy val outerTestNeeded = (
           (expectedTp.prefix ne NoPrefix)
        && !expectedTp.prefix.typeSymbol.isPackageClass
        && needsOuterTest(expectedTp, testedBinder.info, matchOwner)
      )

      // the logic to generate the run-time test that follows from the fact that
      // a `prevBinder` is expected to have type `expectedTp`
      // the actual tree-generation logic is factored out, since the analyses generate Cond(ition)s rather than Trees
      // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
      // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne' will always yield false")
      def renderCondition(cs: TypeTestCondStrategy): cs.Result = {
        import cs._

        // propagate expected type
        def expTp(t: Tree): t.type = t setType expectedTp

        def testedWide              = testedBinder.info.widen
        def expectedWide            = expectedTp.widen
        def isAnyRef                = testedWide <:< AnyRefTpe
        def isAsExpected            = testedWide <:< expectedTp
        def isExpectedPrimitiveType = isAsExpected && isPrimitiveValueType(expectedTp)
        def isExpectedReferenceType = isAsExpected && (expectedTp <:< AnyRefTpe)
        def mkNullTest              = nonNullTest(testedBinder)
        def mkOuterTest             = outerTest(testedBinder, expectedTp)
        def mkTypeTest              = typeTest(testedBinder, expectedWide)

        def mkEqualsTest(lhs: Tree): cs.Result      = equalsTest(lhs, testedBinder)
        def mkEqTest(lhs: Tree): cs.Result          = eqTest(lhs, testedBinder)
        def addOuterTest(res: cs.Result): cs.Result = if (outerTestNeeded) and(res, mkOuterTest) else res

        // If we conform to expected primitive type:
        //   it cannot be null and cannot have an outer pointer. No further checking.
        // If we conform to expected reference type:
        //   have to test outer and non-null
        // If we do not conform to expected type:
        //   have to test type and outer (non-null is implied by successful type test)
        def mkDefault = (
          if (isExpectedPrimitiveType) tru
          else addOuterTest(
            if (isExpectedReferenceType) mkNullTest
            else mkTypeTest
          )
        )

        // true when called to type-test the argument to an extractor
        // don't do any fancy equality checking, just test the type
        // TODO: verify that we don't need to special-case Array
        // I think it's okay:
        //  - the isInstanceOf test includes a test for the element type
        //  - Scala's arrays are invariant (so we don't drop type tests unsoundly)
        if (extractorArgTypeTest) mkDefault
        else expectedTp match {
          case SingleType(_, sym)                       => mkEqTest(gen.mkAttributedQualifier(expectedTp)) // SI-4577, SI-4897
          case ThisType(sym) if sym.isModule            => and(mkEqualsTest(CODE.REF(sym)), mkTypeTest) // must use == to support e.g. List() == Nil
          case ConstantType(Constant(null)) if isAnyRef => mkEqTest(expTp(CODE.NULL))
          case ConstantType(const)                      => mkEqualsTest(expTp(Literal(const)))
          case ThisType(sym)                            => mkEqTest(expTp(This(sym)))
          case _                                        => mkDefault
        }
      }

      val cond = renderCondition(treeCondStrategy)
      val res  = codegen._asInstanceOf(testedBinder, nextBinderTp)

      // is this purely a type test, e.g. no outer check, no equality tests (used in switch emission)
      def isPureTypeTest = renderCondition(pureTypeTestChecker)

      def impliesBinderNonNull(binder: Symbol) = renderCondition(nonNullImpliedByTestChecker(binder))

      override def toString = "TT"+((expectedTp, testedBinder.name, nextBinderTp))
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, override val pos: Position) extends CondTreeMaker {
      val nextBinderTp = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = codegen._equals(patTree, prevBinder)
      val res  = CODE.REF(prevBinder)
      override def toString = "ET"+((prevBinder.name, patTree))
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, var altss: List[List[TreeMaker]], pos: Position) extends TreeMaker with NoNewBinders {
      // don't substitute prevBinder to nextBinder, a set of alternatives does not need to introduce a new binder, simply reuse the previous one

      override private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        super.incorporateOuterSubstitution(outerSubst)
        altss = altss map (alts => propagateSubstitution(alts, substitution))
      }

      def chainBefore(next: Tree)(codegenAlt: Casegen): Tree = {
        atPos(pos){
          // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
          // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
          val combinedAlts = altss map (altTreeMakers =>
            ((casegen: Casegen) => combineExtractors(altTreeMakers :+ TrivialTreeMaker(casegen.one(mkTRUE)))(casegen))
          )

          val findAltMatcher = codegenAlt.matcher(EmptyTree, NoSymbol, BooleanTpe)(combinedAlts, Some(x => mkFALSE))
          codegenAlt.ifThenElseZero(findAltMatcher, substitution(next))
        }
      }
    }

    case class GuardTreeMaker(guardTree: Tree) extends TreeMaker with NoNewBinders {
      val pos = guardTree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = casegen.flatMapGuard(substitution(guardTree), next)
      override def toString = "G("+ guardTree +")"
    }

    // combineExtractors changes the current substitution's of the tree makers in `treeMakers`
    // requires propagateSubstitution(treeMakers) has been called
    def combineExtractors(treeMakers: List[TreeMaker])(casegen: Casegen): Tree =
      treeMakers.foldRight(EmptyTree: Tree)((a, b) => a.chainBefore(b)(casegen))


    def removeSubstOnly(makers: List[TreeMaker]) = makers filterNot (_.isInstanceOf[SubstOnlyTreeMaker])

    // a foldLeft to accumulate the localSubstitution left-to-right
    // it drops SubstOnly tree makers, since their only goal in life is to propagate substitutions to the next tree maker, which is fulfilled by propagateSubstitution
    def propagateSubstitution(treeMakers: List[TreeMaker], initial: Substitution): List[TreeMaker] = {
      var accumSubst: Substitution = initial
      treeMakers foreach { maker =>
        maker incorporateOuterSubstitution accumSubst
        accumSubst = maker.substitution
      }
      removeSubstOnly(treeMakers)
    }

    // calls propagateSubstitution on the treemakers
    def combineCases(scrut: Tree, scrutSym: Symbol, casesRaw: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree = {
      // drops SubstOnlyTreeMakers, since their effect is now contained in the TreeMakers that follow them
      val casesNoSubstOnly = casesRaw map (propagateSubstitution(_, EmptySubstitution))
      combineCasesNoSubstOnly(scrut, scrutSym, casesNoSubstOnly, pt, owner, matchFailGenOverride)
    }

    // pt is the fully defined type of the cases (either pt or the lub of the types of the cases)
    def combineCasesNoSubstOnly(scrut: Tree, scrutSym: Symbol, casesNoSubstOnly: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree =
      fixerUpper(owner, scrut.pos) {
        def matchFailGen = matchFailGenOverride orElse Some(Throw(MatchErrorClass.tpe, _: Tree))

        debug.patmat("combining cases: "+ (casesNoSubstOnly.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        val (suppression, requireSwitch): (Suppression, Boolean) =
          if (settings.XnoPatmatAnalysis) (Suppression.FullSuppression, false)
          else scrut match {
            case Typed(tree, tpt) =>
              val suppressExhaustive = tpt.tpe hasAnnotation UncheckedClass
              val supressUnreachable = tree match {
                case Ident(name) if name startsWith nme.CHECK_IF_REFUTABLE_STRING => true // SI-7183 don't warn for withFilter's that turn out to be irrefutable.
                case _ => false
              }
              val suppression = Suppression(suppressExhaustive, supressUnreachable)
              val hasSwitchAnnotation = treeInfo.isSwitchAnnotation(tpt.tpe)
              // matches with two or fewer cases need not apply for switchiness (if-then-else will do)
              // `case 1 | 2` is considered as two cases.
              def exceedsTwoCasesOrAlts = {
                // avoids traversing the entire list if there are more than 3 elements
                def lengthMax3[T](l: List[T]): Int = l match {
                  case a :: b :: c :: _ => 3
                  case cases =>
                    cases.map({
                      case AlternativesTreeMaker(_, alts, _) :: _ => lengthMax3(alts)
                      case c => 1
                    }).sum
                }
                lengthMax3(casesNoSubstOnly) > 2
              }
              val requireSwitch = hasSwitchAnnotation && exceedsTwoCasesOrAlts
              (suppression, requireSwitch)
            case _ =>
              (Suppression.NoSuppression, false)
          }

        emitSwitch(scrut, scrutSym, casesNoSubstOnly, pt, matchFailGenOverride, unchecked = suppression.suppressExhaustive).getOrElse{
          if (requireSwitch) reporter.warning(scrut.pos, "could not emit switch for @switch annotated match")

          if (casesNoSubstOnly nonEmpty) {
            // before optimizing, check casesNoSubstOnly for presence of a default case,
            // since DCE will eliminate trivial cases like `case _ =>`, even if they're the last one
            // exhaustivity and reachability must be checked before optimization as well
            // TODO: improve notion of trivial/irrefutable -- a trivial type test before the body still makes for a default case
            //   ("trivial" depends on whether we're emitting a straight match or an exception, or more generally, any supertype of scrutSym.tpe is a no-op)
            //   irrefutability checking should use the approximation framework also used for CSE, unreachability and exhaustivity checking
            val synthCatchAll =
              if (casesNoSubstOnly.nonEmpty && {
                    val nonTrivLast = casesNoSubstOnly.last
                    nonTrivLast.nonEmpty && nonTrivLast.head.isInstanceOf[BodyTreeMaker]
                  }) None
              else matchFailGen

            analyzeCases(scrutSym, casesNoSubstOnly, pt, suppression)

            val (cases, toHoist) = optimizeCases(scrutSym, casesNoSubstOnly, pt)

            val matchRes = codegen.matcher(scrut, scrutSym, pt)(cases map combineExtractors, synthCatchAll)

            if (toHoist isEmpty) matchRes else Block(toHoist, matchRes)
          } else {
            codegen.matcher(scrut, scrutSym, pt)(Nil, matchFailGen)
          }
        }
      }

    // TODO: do this during tree construction, but that will require tracking the current owner in treemakers
    // TODO: assign more fine-grained positions
    // fixes symbol nesting, assigns positions
    protected def fixerUpper(origOwner: Symbol, pos: Position) = new Traverser {
      currentOwner = origOwner

      override def traverse(t: Tree) {
        if (t != EmptyTree && t.pos == NoPosition) {
          t.setPos(pos)
        }
        t match {
          case Function(_, _) if t.symbol == NoSymbol =>
            t.symbol = currentOwner.newAnonymousFunctionValue(t.pos)
            debug.patmat("new symbol for "+ ((t, t.symbol.ownerChain)))
          case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == origOwner) =>
            debug.patmat("fundef: "+ ((t, t.symbol.ownerChain, currentOwner.ownerChain)))
            t.symbol.owner = currentOwner
          case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == origOwner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
            debug.patmat("def: "+ ((d, d.symbol.ownerChain, currentOwner.ownerChain)))

            d.symbol.moduleClass andAlso (_.owner = currentOwner)
            d.symbol.owner = currentOwner
          // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
          debug.patmat("untouched "+ ((t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain)))
          case _ =>
        }
        super.traverse(t)
      }

      // override def apply
      // debug.patmat("before fixerUpper: "+ xTree)
      // currentRun.trackerFactory.snapshot()
      // debug.patmat("after fixerupper")
      // currentRun.trackerFactory.snapshot()
    }
  }
}
