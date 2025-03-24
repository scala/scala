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
import scala.tools.nsc.symtab.Flags.{SYNTHETIC, ARTIFACT}
import scala.tools.nsc.Reporting.WarningCategory

/** Translate our IR (TreeMakers) into actual Scala Trees using the factory methods in MatchCodeGen.
 *
 * The IR is mostly concerned with sequencing, substitution, and rendering all necessary conditions.
 */
trait MatchTreeMaking extends MatchCodeGen with Debugging {
  import global._, definitions._, CODE._

  final case class Suppression private (suppressExhaustive: Boolean, suppressUnreachable: Boolean)
  object Suppression {
    val NoSuppression = new Suppression(suppressExhaustive=false, suppressUnreachable=false)
    val FullSuppression = new Suppression(suppressExhaustive=true, suppressUnreachable=true)
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// the making of the trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TreeMakers extends TypedSubstitution with CodegenCore {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, selectorPos: Position): (List[List[TreeMaker]], List[Tree])
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree], unchecked: Boolean): Option[Tree] =
      None

    // for catch (no need to customize match failure)
    def emitTypeSwitch(bindersAndCases: List[(Symbol, List[TreeMaker])], pt: Type): Option[List[CaseDef]] =
      None

    // Exposed separately from emitTypeSwitch, so that we can do the analysis for simple cases where we skip emitTypeSwitch
    def unreachableTypeSwitchCase(cases: List[CaseDef]): Option[CaseDef] =
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
       * (Even though we don't do so anymore -- see scala/bug#5158, scala/bug#5739 and scala/bug#6070.)
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

    /** A dummy tree maker used to mark wildcard patterns.
     * This is later used to back off from exhaustivity checking.
     */
    case object DummyTreeMaker extends TreeMaker with NoNewBinders {
      def pos = EmptyTree.pos

      def chainBefore(next: Tree)(casegen: Casegen): Tree = next
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

      val localSubstitution = Substitution(prevBinder, gen.mkAttributedStableRef(nextBinder))
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
      lazy val localSubstitution = Substitution(List(prevBinder), List(gen.mkAttributedStableRef(nextBinder)))

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
      // mutable case class fields need to be stored regardless (scala/bug#5158, scala/bug#6070) -- see override in ProductExtractorTreeMaker
      // sub patterns bound to wildcard (_) are never stored as they can't be referenced
      // dirty debuggers will have to get dirty to see the wildcards
      private lazy val storedBinders: Set[Symbol] =
        (if (debugInfoEmitVars) subPatBinders.toSet else Set.empty) ++ extraStoredBinders diff ignoredSubPatBinders

      // e.g., mutable fields of a case class in ProductExtractorTreeMaker
      def extraStoredBinders: Set[Symbol]

      def emitVars = storedBinders.nonEmpty

      private lazy val (stored, substed) = subPatBinders.lazyZip(subPatRefs).partition{ case (sym, _) => storedBinders(sym) }

      protected lazy val localSubstitution: Substitution = if (!emitVars) Substitution(subPatBinders, subPatRefs)
        else {
          val (subPatBindersSubstituted, subPatRefsSubstituted) = substed.unzip
          Substitution(subPatBindersSubstituted.toList, subPatRefsSubstituted.toList)
        }

      /** The substitution that specifies the trees that compute the values of the subpattern binders.
       *
       * We pretend to replace the subpattern binders by subpattern refs
       * (Even though we don't do so anymore -- see scala/bug#5158, scala/bug#5739 and scala/bug#6070.)
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
          val typeTraverser = new TypeTraverser {
            def traverse(tp: Type) = {
              tp match {
                case SingleType(_, sym) => ref(sym)
                case _ =>
              }
              mapOver(tp)
            }
          }
          in.foreach {
            case tt: TypeTree => typeTraverser.apply(tt.tpe)
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
     * Make a TreeMaker that performs null check.
     * This is called prior to extractor call.
     */
    case class NonNullTestTreeMaker(
       prevBinder: Symbol,
       expectedTp: Type,
       override val pos: Position) extends FunTreeMaker {
      override lazy val nextBinder = prevBinder.asTerm // just passing through
      val nextBinderTp = nextBinder.info.widen

      val nullCheck = REF(prevBinder) OBJ_NE NULL
      lazy val localSubstitution = Substitution(Nil, Nil)

      def skipNullTest = isPrimitiveValueType(expectedTp) || expectedTp.typeSymbol.isDerivedValueClass

      def chainBefore(next: Tree)(casegen: Casegen): Tree =
        atPos(pos) {
          if (skipNullTest) next
          else casegen.ifThenElseZero(nullCheck, next)
        }

      override def toString = s"NN(${prevBinder.name})"
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
     * This makes extractor patterns more debuggable (scala/bug#5739).
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
          if (extractorReturnsBoolean) casegen.flatMapCond(extractor, UNIT, nextBinder, condAndNext)
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
     * This makes extractor patterns more debuggable (scala/bug#5739) as well as
     * avoiding mutation after the pattern has been matched (scala/bug#5158, scala/bug#6070)
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
          val ignoredSubPatBinders: Set[Symbol]
         ) extends FunTreeMaker with PreserveSubPatBinders {

      val nextBinder = prevBinder // just passing through

      // mutable binders must be stored to avoid unsoundness or seeing mutation of fields after matching (scala/bug#5158, scala/bug#6070)
      def extraStoredBinders: Set[Symbol] = mutableBinders.toSet

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        extraCond match {
          case Some(cond) =>
            casegen.ifThenElseZero(cond, bindSubPats(substitution(next)))
          case _ =>
            bindSubPats(substitution(next))
        }
      }

      override def toString = s"P(${prevBinder.name}, ${extraCond.fold("")(_.toString)}, ${localSubstitution})"
    }

    object TypeTestTreeMaker {
      // factored out so that we can consistently generate other representations of the tree that implements the test
      // (e.g. propositions for exhaustivity and friends, boolean for isPureTypeTest)
      trait TypeTestCondStrategy {
        type Result

        def withOuterTest(orig: Result)(@unused testedBinder: Symbol, @unused expectedTp: Type): Result = orig
        // TODO: can probably always widen
        def typeTest(testedBinder: Symbol, expectedTp: Type): Result
        def nonNullTest(testedBinder: Symbol): Result
        def equalsTest(pat: Tree, testedBinder: Symbol): Result
        def eqTest(pat: Tree, testedBinder: Symbol): Result
        def and(a: Result, b: Result): Result
        def tru: Result
      }

      object treeCondStrategy extends TypeTestCondStrategy {
        type Result = Tree

        def and(a: Result, b: Result): Result                = a AND b
        def tru                                              = TRUE
        def typeTest(testedBinder: Symbol, expectedTp: Type) = codegen._isInstanceOf(testedBinder, expectedTp)
        def nonNullTest(testedBinder: Symbol)                = REF(testedBinder) OBJ_NE NULL
        def equalsTest(pat: Tree, testedBinder: Symbol)      = codegen._equals(pat, testedBinder)
        def eqTest(pat: Tree, testedBinder: Symbol)          = REF(testedBinder) OBJ_EQ pat

        override def withOuterTest(orig: Tree)(testedBinder: Symbol, expectedTp: Type): Tree = {
          // Check if a type is defined in a static location. Unlike `tp.isStatic` before `flatten`,
          // this also includes methods and (possibly nested) objects inside of methods.
          def definedInStaticLocation(tp: Type): Boolean = {
            @tailrec
            def isStatic(tp: Type): Boolean =
              if (tp == NoType || tp.typeSymbol.isPackageClass || tp == NoPrefix || nme.isReplWrapperName(tp.typeSymbol.name)) true
              else if (tp.typeSymbol.isModuleClass) isStatic(tp.prefix)
              else false
            tp.typeSymbol.owner == tp.prefix.typeSymbol && isStatic(tp.prefix)
          }

          // In `def foo(a: b.B) = a match { case _: p.P }`
          // testedBinder.symbol.info = b.B
          // expectedTp               = p.P

          expectedTp.dealias match {
            case RefinedType(Nil, _) => orig
            case rt@RefinedType(parent :: rest, scope) =>
              // If the pattern type is refined type, emit outer tests for each component.
              withOuterTest(withOuterTest(orig)(testedBinder, parent))(testedBinder, copyRefinedType(rt, rest, scope))
            case expectedTp =>
              val expectedClass = expectedTp.typeSymbol
              // .typeSymbol dealiases, so look at the prefix of the base type at the dealiased symbol,
              // not of expectedTp itself.
              val expectedPrefix = expectedTp.baseType(expectedClass).prefix


              // Given `(a: x.B) match { case _: x.P }` where P is subclass of B, is it possible
              // that a value conforms to both x.B and x1.P where `x ne x1`?
              //
              // To answer this, we create a new prefix based on a fresh symbol and check the
              // base type of TypeRef(freshPrefix, typePatternSymbol (P), args) at the binder
              // symbol (B). If that is prefixed by the fresh symbol, they are statically the
              // same.
              //
              // It is not sufficient to show that x.P is a subtype of x.B, as this
              // would incorrectly elide the outer test in:
              //
              // class P extends p1.B
              // def test(b: p1.B) = b match { case _: p1.P }
              // test(new p2.P)
              def prefixAligns: Boolean = {
                expectedTp match {
                  case TypeRef(pre, _, _) if !pre.isStable => // e.g. _: Outer#Inner
                    false
                  case TypeRef(pre, sym, args) =>
                    val testedBinderClass = testedBinder.info.baseClasses.find { sym =>
                      sym.isClass && !sym.isRefinementClass
                    }.getOrElse(NoSymbol)
                    val testedBinderType = testedBinder.info.baseType(testedBinderClass)

                    val testedPrefixIsExpectedTypePrefix = pre =:= testedBinderType.prefix
                    val testedPrefixAndExpectedPrefixAreStaticallyIdentical: Boolean = {
                      def check(freshPrefix: Type): Boolean = {
                        val expectedTpFromFreshPrefix = TypeRef(freshPrefix, sym, args)
                        val baseTypeFromFreshPrefix = expectedTpFromFreshPrefix.baseType(testedBinderClass)
                        freshPrefix eq baseTypeFromFreshPrefix.prefix
                      }
                      pre match {
                        case ThisType(thissym) =>
                          check(ThisType(thissym.cloneSymbol(thissym.owner)))
                        case _ =>
                          pre.termSymbol match {
                            case NoSymbol => false
                            case preSym =>
                              val freshPreSym = preSym.cloneSymbol(preSym.owner).setInfo(preSym.info)
                              check(singleType(pre.prefix, freshPreSym))
                          }
                      }

                    }
                    testedPrefixAndExpectedPrefixAreStaticallyIdentical && testedPrefixIsExpectedTypePrefix
                  case _ =>
                    false
                }
              }

              if ((expectedPrefix eq NoPrefix)
                || expectedTp.typeSymbol.isJava
                || definedInStaticLocation(expectedTp)
                || testedBinder.info <:< expectedTp
                || prefixAligns) orig
              else gen.mkAttributedQualifierIfPossible(expectedPrefix) match {
                case None => orig
                case Some(expectedOuterRef) =>
                  // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix`
                  // by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
                  // if there's an outer accessor, otherwise the condition becomes `true`
                  // TODO: centralize logic whether there's an outer accessor and use here?
                  val synthOuterGetter = expectedTp.typeSymbol.newMethod(nme.OUTER_SYNTH, newFlags = SYNTHETIC | ARTIFACT) setInfo expectedPrefix
                  val outerTest = (Select(codegen._asInstanceOf(testedBinder, expectedTp), synthOuterGetter)) OBJ_EQ expectedOuterRef
                  and(orig, outerTest)
              }
          }
        }
      }

      object pureTypeTestChecker extends TypeTestCondStrategy {
        type Result = Boolean

        def typeTest(testedBinder: Symbol, expectedTp: Type): Result  = true

        def nonNullTest(testedBinder: Symbol): Result                 = false
        def equalsTest(pat: Tree, testedBinder: Symbol): Result       = false
        def eqTest(pat: Tree, testedBinder: Symbol): Result           = false
        def and(a: Result, b: Result): Result                         = false // we don't and type tests, so the conjunction must include at least one false
        def tru                                                       = true
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
          // https://github.com/scala/bug/issues/4577 "pattern matcher, still disappointing us at equality time"

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

      // the logic to generate the run-time test that follows from the fact that
      // a `prevBinder` is expected to have type `expectedTp`
      // the actual tree-generation logic is factored out, since the analyses generate Cond(ition)s rather than Trees
      // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
      // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne` will always yield false")
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
        def mkTypeTest              = typeTest(testedBinder, expectedWide)

        def mkEqualsTest(lhs: Tree): cs.Result      = equalsTest(lhs, testedBinder)
        def mkEqTest(lhs: Tree): cs.Result          = eqTest(lhs, testedBinder)
        def addOuterTest(res: cs.Result): cs.Result = withOuterTest(res)(testedBinder, expectedTp)

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
          case SingleType(_, sym)                       =>
            val expected = gen.mkAttributedQualifier(expectedTp) // scala/bug#4577, scala/bug#4897
            if (expectedTp <:< AnyRefTpe) mkEqTest(expected)
            else mkEqualsTest(expected)
          // Should revisit if we end up lifting `eq`'s definition to `Any`, as discussed here:
          // https://groups.google.com/d/msg/scala-internals/jsVlJI4H5OQ/8emZWRmgzcoJ
          case ThisType(sym) if sym.isModule            => and(mkEqualsTest(REF(sym)), mkTypeTest) // must use == to support e.g. List() == Nil
          case ConstantType(Constant(null)) if isAnyRef => mkEqTest(expTp(NULL))
          case ConstantType(const)                      => mkEqualsTest(expTp(Literal(const)))
          case ThisType(sym)                            => mkEqTest(expTp(This(sym)))
          case _                                        => mkDefault
        }
      }

      val cond = renderCondition(treeCondStrategy)
      val res  = codegen._asInstanceOf(testedBinder, nextBinderTp)

      // is this purely a type test, e.g. no outer check, no equality tests (used in switch emission)
      def isPureTypeTest = renderCondition(pureTypeTestChecker)

      override def toString = "TT"+((expectedTp, testedBinder.name, nextBinderTp))
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, override val pos: Position) extends CondTreeMaker {
      val nextBinderTp = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = codegen._equals(patTree, prevBinder)
      val res  = gen.mkAttributedStableRef(prevBinder)
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
            ((casegen: Casegen) => combineExtractors(altTreeMakers :+ TrivialTreeMaker(casegen.one(TRUE)))(casegen))
          )

          val findAltMatcher = codegenAlt.matcher(EmptyTree, NoSymbol, BooleanTpe)(combinedAlts, Some(_ => FALSE))
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
    def removeDummy(makers: List[TreeMaker]) = makers filterNot (_ == DummyTreeMaker)

    // a foldLeft to accumulate the localSubstitution left-to-right
    // it drops SubstOnly tree makers, since their only goal in life is to propagate substitutions to the next tree maker, which is fulfilled by propagateSubstitution
    def propagateSubstitution(treeMakers: List[TreeMaker], initial: Substitution): List[TreeMaker] = {
      var accumSubst: Substitution = initial
      removeDummy(treeMakers) foreach { maker =>
        maker incorporateOuterSubstitution accumSubst
        accumSubst = maker.substitution
      }
      removeSubstOnly(removeDummy(treeMakers))
    }

    def getSuppression(scrut: Tree): Suppression =
      if (settings.XnoPatmatAnalysis.value) Suppression.FullSuppression
      else scrut match {
        case Typed(tree, tpt) =>
          val suppressExhaustive  = tpt.tpe.hasAnnotation(UncheckedClass)
          val suppressUnreachable = tree match {
            // scala/bug#7183 don't warn for withFilter's that turn out to be irrefutable.
            case Ident(name) => name.startsWith(nme.CHECK_IF_REFUTABLE_STRING)
            case _ => false
          }
          Suppression(suppressExhaustive, suppressUnreachable)
        case _ => Suppression.NoSuppression
      }

    def requiresSwitch(scrut: Tree, cases: List[List[TreeMaker]]): Boolean = {
      if (settings.XnoPatmatAnalysis.value) false
      else scrut match {
        case Typed(_, tpt) =>
          val hasSwitchAnnotation = treeInfo.isSwitchAnnotation(tpt.tpe)
          // matches with two or fewer cases need not apply for switchiness (if-then-else will do)
          // `case 1 | 2` is considered as two cases.
          def exceedsTwoCasesOrAlts = {
            // avoids traversing the entire list if there are more than 3 elements
            def lengthMax3(cases: List[List[TreeMaker]]): Int = cases match {
              case _ :: _ :: _ :: _ => 3
              case cases            => cases.map {
                case AlternativesTreeMaker(_, alts, _) :: _ => lengthMax3(alts)
                case _                                      => 1
              }.sum
            }
            lengthMax3(cases) > 2
          }
          hasSwitchAnnotation && exceedsTwoCasesOrAlts
        case _ => false
      }
    }

    // See the use of RegularSwitchMaker by SwitchEmission#emitSwitch, which this code emulates or duplicates.
    private object Switchable {
      val switchableTpe = Set(ByteTpe, ShortTpe, IntTpe, CharTpe, StringTpe)

      def apply(scrutSym: Symbol, cases: List[List[TreeMaker]]): Boolean = switchableTpe(scrutSym.tpe.dealiasWiden) && {
        def switchable(tms: List[TreeMaker]): Boolean =
          tms.forall {
            case EqualityTestTreeMaker(_, SwitchablePattern(), _) => true
            case AlternativesTreeMaker(_, altss, _) => Switchable(scrutSym, altss)
            case BodyTreeMaker(_, _) => true
            case _ => false
          }
        cases.forall(switchable)
      }

      object SwitchablePattern {
        def unapply(pat: Tree): Boolean = pat.tpe match {
          case const: ConstantType => const.value.isIntRange || const.value.tag == StringTag || const.value.tag == NullTag
          case _ => false
        }
      }
    }

    // pt is the fully defined type of the cases (either pt or the lub of the types of the cases)
    def combineCases(
        scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type,
        selectorPos: Position, owner: Symbol, matchFailGenOverride: Option[Tree => Tree],
        suppression: Suppression,
    ): Tree =
      fixerUpper(owner, scrut.pos) {
        def matchFailGen = matchFailGenOverride orElse Some(Throw(MatchErrorClass.tpe, _: Tree))

        debug.patmat("combining cases: "+ (cases.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        emitSwitch(scrut, scrutSym, cases, pt, matchFailGenOverride, unchecked = suppression.suppressExhaustive).getOrElse {
          if (requiresSwitch(scrut, cases))
            typer.context.warning(scrut.pos, "could not emit switch for @switch annotated match", WarningCategory.OtherMatchAnalysis)

          // If cases are switchable, suppress warning for exhaustivity.
          // The switch was not emitted, probably because there aren't enough cases.
          val suppression1 =
            if (Switchable(scrutSym, cases)) suppression.copy(suppressExhaustive = true)
            else suppression

          if (!cases.isEmpty) {
            // before optimizing, check cases for presence of a default case,
            // since DCE will eliminate trivial cases like `case _ =>`, even if they're the last one
            // exhaustivity and reachability must be checked before optimization as well
            // TODO: improve notion of trivial/irrefutable -- a trivial type test before the body still makes for a default case
            //   ("trivial" depends on whether we're emitting a straight match or an exception, or more generally, any supertype of scrutSym.tpe is a no-op)
            //   irrefutability checking should use the approximation framework also used for CSE, unreachability and exhaustivity checking
            val synthCatchAll = cases match {
              case _ :+ Seq(_: BodyTreeMaker, _*) => None
              case _                              => matchFailGen
            }

            analyzeCases(scrutSym, cases, pt, suppression1)

            val (optimizedCases, toHoist) = optimizeCases(scrutSym, cases, pt, selectorPos)

            val matchRes = codegen.matcher(scrut, scrutSym, pt)(optimizedCases map combineExtractors, synthCatchAll)

            if (toHoist.isEmpty) matchRes else Block(toHoist, matchRes)
          } else {
            codegen.matcher(scrut, scrutSym, pt)(Nil, matchFailGen)
          }
        }
      }

    // TODO: do this during tree construction, but that will require tracking the current owner in treemakers
    // TODO: assign more fine-grained positions
    // fixes symbol nesting, assigns positions
    protected def fixerUpper(origOwner: Symbol, pos: Position) = new InternalTraverser {
      currentOwner = origOwner

      override def traverse(t: Tree): Unit = {
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
          //   debug.patmat("untouched "+ ((t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain)))
          case _ =>
        }
        t.traverse(this)
      }

      // override def apply
      // debug.patmat("before fixerUpper: "+ xTree)
      // currentRun.trackerFactory.snapshot()
      // debug.patmat("after fixerupper")
      // currentRun.trackerFactory.snapshot()
    }
  }
}
