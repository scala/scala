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
package transform.patmat

import scala.annotation._

/** Translate typed Trees that represent pattern matches into the patternmatching IR, defined by TreeMakers.
 */
trait MatchTranslation {
  self: PatternMatching =>

  import global._
  import definitions._
  import treeInfo.{ Unapplied, unbind }
  import CODE._

  // Always map repeated params to sequences
  private def setVarInfo(sym: Symbol, info: Type) =
    sym setInfo debug.patmatResult(s"changing ${sym.defString} to")(repeatedToSeq(info))

  trait MatchTranslator extends TreeMakers with TreeMakerWarnings {
    import typer.context
    def selectorPos: Position

    /** A conservative approximation of which patterns do not discern anything.
     * They are discarded during the translation.
     */
    object WildcardPattern {
      def unapply(pat: Tree): Boolean = pat match {
        case Bind(nme.WILDCARD, WildcardPattern()) => true // don't skip when binding an interesting symbol!
        case Star(WildcardPattern())               => true
        case x: Ident                              => treeInfo.isVarPattern(x)
        case Alternative(ps)                       => ps forall unapply
        case EmptyTree                             => true
        case _                                     => false
      }
    }

    object PatternBoundToUnderscore {
      def unapply(pat: Tree): Boolean = pat match {
        case Bind(nme.WILDCARD, _)                => true // don't skip when binding an interesting symbol!
        case Ident(nme.WILDCARD)                  => true
        case Alternative(ps)                      => ps forall unapply
        case Typed(PatternBoundToUnderscore(), _) => true
        case _                                    => false
      }
    }

    object SymbolBound {
      def unapply(tree: Tree): Option[(Symbol, Tree)] = tree match {
        case Bind(_, expr) if tree.hasExistingSymbol => Some(tree.symbol -> expr)
        case _                                       => None
      }
    }

    final case class BoundTree(binder: Symbol, tree: Tree) {
      private lazy val extractor = ExtractorCall(tree)

      def pos     = tree.pos
      def tpe     = binder.info.dealias // the type of the variable bound to the pattern
      def pt      = unbound match {
        case Star(tpt)      => seqType(tpt.tpe)
        case TypeBound(tpe) => tpe
        case unbound        => unbound.tpe
      }

      object SymbolAndTypeBound {
        def unapply(tree: Tree): Option[(Symbol, Type)] = tree match {
          case SymbolBound(sym, TypeBound(tpe)) => Some(sym -> tpe)
          case TypeBound(tpe)                   => Some(binder -> tpe)
          case _                                => None
        }
      }

      object TypeBound {
        def unapply(tree: Tree): Option[Type] = tree match {
          case Typed(Ident(_), _) if tree.tpe != null => Some(tree.tpe)
          case _                                      => None
        }
      }

      private def rebindTo(pattern: Tree) = BoundTree(binder, pattern)
      private def step(treeMakers: TreeMaker*)(subpatterns: BoundTree*): TranslationStep = TranslationStep(treeMakers.toList, subpatterns.toList)

      private def bindingStep(sub: Symbol, subpattern: Tree) = step(SubstOnlyTreeMaker(sub, binder))(rebindTo(subpattern))
      private def equalityTestStep()                         = step(EqualityTestTreeMaker(binder, tree, pos))()
      private def typeTestStep(sub: Symbol, subPt: Type)     = step(TypeTestTreeMaker(sub, binder, subPt, subPt)(pos))()
      private def alternativesStep(alts: List[Tree])         = step(AlternativesTreeMaker(binder, translatedAlts(alts), alts.head.pos))()
      private def translatedAlts(alts: List[Tree])           = alts map (alt => rebindTo(alt).translate())
      private def noStep()                                   = step(DummyTreeMaker)()

      private def unsupportedPatternMsg = sm"""
        |unsupported pattern: ${tree.shortClass} / $this (this is a scalac bug.)
        |""".trim

      // example check: List[Int] <:< ::[Int]
      private def extractorStep(): TranslationStep = {
        import extractor.treeMakers

        // paramType = the type expected by the unapply
        // TODO: paramType may contain unbound type params (run/t2800, run/t3530)
        val (makers, unappBinder) = {
          val paramType = extractor.expectedExtractedType
          // Statically conforms to paramType
          if (tpe <:< paramType) {
            // enforce all extractor patterns to be non-null
            val nonNullTest = NonNullTestTreeMaker(binder, paramType, pos)
            val unappBinder = nonNullTest.nextBinder
            (nonNullTest :: treeMakers(unappBinder, pos), unappBinder)
          }
          else {
            // chain a type-testing extractor before the actual extractor call
            // it tests the type, checks the outer pointer and casts to the expected type
            // TODO: the outer check is mandated by the spec for case classes, but we do it for user-defined unapplies as well [SPEC]
            // (the prefix of the argument passed to the unapply must equal the prefix of the type of the binder)
            val typeTest = TypeTestTreeMaker(binder, binder, paramType, paramType)(pos, extractorArgTypeTest = true)
            // binder is known non-null because the type test would not succeed on `null`
            val unappBinder = typeTest.nextBinder
            (typeTest :: treeMakers(unappBinder, pos), unappBinder)
          }
        }

        foreach2(extractor.subBoundTrees, extractor.subPatTypes(unappBinder)) { (bt, pt) =>
          setVarInfo(bt.binder, pt)
        }

        step(makers: _*)(extractor.subBoundTrees: _*)
      }

      // Summary of translation cases. I moved the excerpts from the specification further below so all
      // the logic can be seen at once.
      //
      // [1] skip wildcard trees -- no point in checking them
      // [2] extractor and constructor patterns
      // [3] replace subpatBinder by patBinder, as if the Bind was not there.
      //     It must be patBinder, as subpatBinder has the wrong info: even if the bind assumes a better type,
      //     this is not guaranteed until we cast
      // [4] typed patterns - a typed pattern never has any subtrees
      //     must treat Typed and Bind together -- we need to know the patBinder of the Bind pattern to get at the actual type
      // [5] literal and stable id patterns
      // [6] pattern alternatives
      // [7] symbol-less bind patterns - this happens in certain ill-formed programs, there'll be an error later
      //     don't fail here though (or should we?)
      def nextStep(): TranslationStep = tree match {
        case WildcardPattern()                                        => noStep()
        case _: UnApply | _: Apply                                    => extractorStep()
        case SymbolAndTypeBound(sym, tpe)                             => typeTestStep(sym, tpe)
        case TypeBound(tpe)                                           => typeTestStep(binder, tpe)
        case SymbolBound(sym, expr)                                   => bindingStep(sym, expr)
        case Literal(Constant(_)) | Ident(_) | Select(_, _) | This(_) => equalityTestStep()
        case Alternative(alts)                                        => alternativesStep(alts)
        case _                                                        => reporter.error(pos, unsupportedPatternMsg) ; noStep()
      }
      def translate(): List[TreeMaker] = nextStep() merge (_.translate())

      private def concreteType = tpe.upperBound
      private def unbound = unbind(tree)
      private def tpe_s = if (pt <:< concreteType) "" + pt else s"$pt (binder: $tpe)"
      private def at_s = unbound match {
        case WildcardPattern() => ""
        case pat               => s" @ $pat"
      }
      override def toString = s"${binder.name}: $tpe_s$at_s"
    }

    // a list of TreeMakers that encode `patTree`, and a list of arguments for recursive invocations of `translatePattern` to encode its subpatterns
    final case class TranslationStep(makers: List[TreeMaker], subpatterns: List[BoundTree]) {
      def merge(f: BoundTree => List[TreeMaker]): List[TreeMaker] = makers ::: (subpatterns flatMap f)
      override def toString = if (subpatterns.isEmpty) "" else subpatterns.mkString("(", ", ", ")")
    }

    /** NOTE: the resulting tree is not type checked, nor are nested pattern matches transformed
      *   thus, you must typecheck the result (and that will in turn translate nested matches)
      *   this could probably be optimized...
      */
    def translateMatch(match_ : Match): Tree = {
      val Match(selector, cases) = match_

      val (nonSyntheticCases, defaultOverride) = cases match {
        case init :+ last if treeInfo isSyntheticDefaultCase last => (init, Some(((_: Tree) => last.body)))
        case _                                                    => (cases, None)
      }

      if (!settings.XnoPatmatAnalysis.value) checkMatchVariablePatterns(nonSyntheticCases)

      // we don't transform after uncurry
      // (that would require more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      if (phase.id >= currentRun.uncurryPhase.id)
        devWarning(s"running translateMatch past uncurry (at $phase) on $selector match $cases")

      debug.patmat("translating "+ cases.mkString("{", "\n", "}"))

      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.patmatNanos) else null

      val selectorTp = repeatedToSeq(elimAnonymousClass(selector.tpe.withoutAnnotations))

      // when one of the internal cps-type-state annotations is present, strip all CPS annotations
      val origPt  = removeCPSFromPt(match_.tpe)
      // relevant test cases: pos/existentials-harmful.scala, pos/gadt-gilles.scala, pos/t2683.scala, pos/virtpatmat_exist4.scala
      // pt is the skolemized version
      val pt = repeatedToSeq(origPt)

      // val packedPt = repeatedToSeq(typer.packedType(match_, context.owner))
      val selectorSym = freshSym(selector.pos, selectorTp) setFlag treeInfo.SYNTH_CASE_FLAGS

      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala
      val combined = combineCases(selector, selectorSym, nonSyntheticCases map translateCase(selectorSym, pt), pt, selectorPos, matchOwner, defaultOverride, getSuppression(selector))

      if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.patmatNanos, start)
      combined
    }

    // return list of typed CaseDefs that are supported by the backend (typed/bind/wildcard)
    // we don't have a global scrutinee -- the caught exception must be bound in each of the casedefs
    // there's no need to check the scrutinee for null -- "throw null" becomes "throw new NullPointerException"
    // try to simplify to a type-based switch, or fall back to a catch-all case that runs a normal pattern match
    // unlike translateMatch, we type our result before returning it
    def translateTry(caseDefs: List[CaseDef], pt: Type, pos: Position): List[CaseDef] =
      // if they're already simple enough to be handled by the back-end, we're done
      if (caseDefs forall treeInfo.isCatchCase) {
        // well, we do need to look for unreachable cases
        if (!settings.XnoPatmatAnalysis.value) unreachableTypeSwitchCase(caseDefs).foreach(cd => reportUnreachable(cd.body.pos))

        caseDefs
      } else {
        val swatches = { // switch-catches
          // scala/bug#7459 must duplicate here as we haven't committed to switch emission, and just figuring out
          //         if we can ends up mutating `caseDefs` down in the use of `substituteSymbols` in
          //         `TypedSubstitution#Substitution`. That is called indirectly by `emitTypeSwitch`.
          val bindersAndCases = caseDefs.map(_.duplicate) map { caseDef =>
            // generate a fresh symbol for each case, hoping we'll end up emitting a type-switch (we don't have a global scrut there)
            // if we fail to emit a fine-grained switch, have to do translateCase again with a single scrutSym (TODO: uniformize substitution on treemakers so we can avoid this)
            val caseScrutSym = freshSym(caseDef.pat.pos, ThrowableTpe)
            (caseScrutSym, translateCase(caseScrutSym, pt)(caseDef))
          }

          for(cases <- emitTypeSwitch(bindersAndCases, pt).toList
              if cases forall treeInfo.isCatchCase; // must check again, since it's not guaranteed -- TODO: can we eliminate this? e.g., a type test could test for a trait or a non-trivial prefix, which are not handled by the back-end
              cse <- cases) yield fixerUpper(matchOwner, pos)(cse)
        }

        val catches = if (swatches.nonEmpty) swatches else {
          val scrutSym = freshSym(caseDefs.head.pat.pos, ThrowableTpe)
          val cases    = caseDefs.map(translateCase(scrutSym, pt))
          val casesPos = wrappingPos(caseDefs)

          val exSym = freshSym(pos, ThrowableTpe, "ex")
          val suppression =
            if (settings.XnoPatmatAnalysis.value) Suppression.FullSuppression
            else Suppression.NoSuppression.copy(suppressExhaustive = true) // try/catches needn't be exhaustive

          val combo = combineCases(REF(exSym), scrutSym, cases, pt, selectorPos, matchOwner, Some(_ => Throw(REF(exSym))), suppression)
          List(atPos(casesPos) {
            CaseDef(
              Bind(exSym, Ident(nme.WILDCARD)), // TODO: does this need fixing upping?
              EmptyTree,
              combo
            )
          })
        }

        typer.typedCases(catches, ThrowableTpe, WildcardType)
      }

    /**  The translation of `pat if guard => body` has two aspects:
      *     1) the substitution due to the variables bound by patterns
      *     2) the combination of the extractor calls using `flatMap`.
      *
      * 2) is easy -- it looks like: `translatePattern_1.flatMap(translatePattern_2....flatMap(translatePattern_N.flatMap(translateGuard.flatMap((x_i) => success(Xbody(x_i)))))...)`
      *     this must be right-leaning tree, as can be seen intuitively by considering the scope of bound variables:
      *     variables bound by pat_1 must be visible from the function inside the left-most flatMap right up to Xbody all the way on the right
      * 1) is tricky because translatePattern_i determines the shape of translatePattern_i+1:
      *    zoom in on `translatePattern_1.flatMap(translatePattern_2)` for example -- it actually looks more like:
      *      `translatePattern_1(x_scrut).flatMap((x_1) => {y_i -> x_1._i}translatePattern_2)`
      *
      *    `x_1` references the result (inside the monad) of the extractor corresponding to `pat_1`,
      *    this result holds the values for the constructor arguments, which translatePattern_1 has extracted
      *    from the object pointed to by `x_scrut`. The `y_i` are the symbols bound by `pat_1` (in order)
      *    in the scope of the remainder of the pattern, and they must thus be replaced by:
      *      - (for 1-ary unapply) x_1
      *      - (for n-ary unapply, n > 1) selection of the i'th tuple component of `x_1`
      *      - (for unapplySeq) x_1.apply(i)
      *
      *    in the treemakers,
      *
      *    Thus, the result type of `translatePattern_i`'s extractor must conform to `M[(T_1,..., T_n)]`.
      *
      *    Operationally, phase 1) is a foldLeft, since we must consider the depth-first-flattening of
      *    the transformed patterns from left to right. For every pattern ast node, it produces a transformed ast and
      *    a function that will take care of binding and substitution of the next ast (to the right).
      *
      */
    def translateCase(scrutSym: Symbol, pt: Type)(caseDef: CaseDef): List[TreeMaker] = {
      val CaseDef(pattern, guard, body) = caseDef
      val treeMakers = translatePattern(BoundTree(scrutSym, pattern)) ++ translateGuard(guard) :+ translateBody(body, pt)
      propagateSubstitution(treeMakers, EmptySubstitution)
    }

    def translatePattern(bound: BoundTree): List[TreeMaker] = bound.translate()

    def translateGuard(guard: Tree): List[TreeMaker] =
      if (guard == EmptyTree) Nil
      else List(GuardTreeMaker(guard))

    // TODO: body.tpe is the type of the body after applying the substitution that represents the solution of GADT type inference
    // need the explicit cast in case our substitutions in the body change the type to something that doesn't take GADT typing into account
    def translateBody(body: Tree, matchPt: Type): TreeMaker =
      BodyTreeMaker(body, matchPt)

    // Some notes from the specification

    /*A constructor pattern is of the form c(p1, ..., pn) where n ≥ 0.
      It consists of a stable identifier c, followed by element patterns p1, ..., pn.
      The constructor c is a simple or qualified name which denotes a case class (§5.3.2).

      If the case class is monomorphic, then it must conform to the expected type of the pattern,
      and the formal parameter types of x’s primary constructor (§5.3) are taken as the expected
      types of the element patterns p1, ..., pn.

      If the case class is polymorphic, then its type parameters are instantiated so that the
      instantiation of c conforms to the expected type of the pattern.
      The instantiated formal parameter types of c’s primary constructor are then taken as the
      expected types of the component patterns p1, ..., pn.

      The pattern matches all objects created from constructor invocations c(v1, ..., vn)
      where each element pattern pi matches the corresponding value vi .
      A special case arises when c’s formal parameter types end in a repeated parameter.
      This is further discussed in (§8.1.9).
    **/

    /* A typed pattern x : T consists of a pattern variable x and a type pattern T.
       The type of x is the type pattern T, where each type variable and wildcard is replaced by a fresh, unknown type.
       This pattern matches any value matched by the type pattern T (§8.2); it binds the variable name to that value.
    */

    /* A pattern binder x@p consists of a pattern variable x and a pattern p.
       The type of the variable x is the static type T of the pattern p.
       This pattern matches any value v matched by the pattern p,
       provided the run-time type of v is also an instance of T,  <-- TODO! https://github.com/scala/bug/issues/1503
       and it binds the variable name to that value.
    */

    /* 8.1.4 Literal Patterns
         A literal pattern L matches any value that is equal (in terms of ==) to the literal L.
         The type of L must conform to the expected type of the pattern.

       8.1.5 Stable Identifier Patterns  (a stable identifier r (see §3.1))
         The pattern matches any value v such that r == v (§12.1).
         The type of r must conform to the expected type of the pattern.
    */


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// helper methods: they analyze types and trees in isolation, but they are not (directly) concerned with the structure of the overall translation
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    object ExtractorCall {
      // TODO: check unargs == args
      def apply(tree: Tree): ExtractorCall = tree match {
        case UnApply(unfun@Unapplied(fun), args) => new ExtractorCallRegular(fun, args)(unfun) // extractor
        case Apply(fun, args)                    => new ExtractorCallProd(fun, args)      // case class
        case x                                   => throw new MatchError(x)
      }
    }

    abstract class ExtractorCall(fun: Tree, args: List[Tree]) extends ExtractorAlignment(fun, args)(context) {
      /** Create the TreeMaker that embodies this extractor call
       */
      def treeMakers(binder: Symbol, pos: Position): List[TreeMaker]

      // `subPatBinders` are the variables bound by this pattern in the following patterns
      // subPatBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
      // must set infos to `subPatTypes`, which are provided by extractor's result,
      // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
      // (it will later result in a type test when `tp` is not a subtype of `b.info`)
      // TODO: can we simplify this, together with the Bound case?
      def subPatBinders = subBoundTrees map (_.binder)
      lazy val subBoundTrees: List[BoundTree] = args map {
        case SymbolBound(sym, expr) => BoundTree(sym, expr)
        case tree                   => BoundTree(freshSym(tree.pos, prefix = "p"), tree)
      }

      // never store these in local variables (for PreserveSubPatBinders)
      lazy val ignoredSubPatBinders: Set[Symbol] = (subPatBinders zip args).collect { case (b, PatternBoundToUnderscore()) => b }.toSet

      // there are `productArity` non-seq elements in the tuple.
      protected def firstIndexingBinder = productArity
      protected def expectedLength      = elementArity
      protected def lastIndexingBinder  = nonStarArity - 1

      private def productElemsToN(binder: Symbol, n: Int): List[Tree] = if (n == 0) Nil else List.tabulate(n)(i => tupleSel(binder)(i + 1))
      private def genTake(binder: Symbol, n: Int): List[Tree]         = if (n == 0) Nil else List.tabulate(n)(codegen index seqTree(binder, forceImmutable = false))
      private def genDrop(binder: Symbol, n: Int): List[Tree]         = codegen.drop(seqTree(binder, forceImmutable = false))(n) :: Nil

      // codegen.drop(seqTree(binder))(nbIndexingIndices)))).toList
      protected def seqTree(binder: Symbol, @unused forceImmutable: Boolean) = tupleSel(binder)(firstIndexingBinder + 1)
      protected def tupleSel(binder: Symbol)(i: Int): Tree           = codegen.tupleSel(binder)(i)

      // the trees that select the subpatterns on the extractor's result,
      // referenced by `binder`
      protected def subPatRefsSeq(binder: Symbol): List[Tree] = {
        def lastTrees: List[Tree] = {
          if (!isStar) Nil
          else if (expectedLength == 0) seqTree(binder, forceImmutable = true) :: Nil
          else genDrop(binder, expectedLength)
        }
        // this error-condition has already been checked by checkStarPatOK:
        //   if(isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if(lastIsStar) 1 else 0) == totalArity, "(resultInMonad, ts, subPatTypes, subPats)= "+(resultInMonad, ts, subPatTypes, subPats))

        // [1] there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
        // [2] then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
        // [3] the last one -- if the last subpattern is a sequence wildcard:
        //       drop the prefix (indexed by the refs on the preceding line), return the remainder
        (    productElemsToN(binder, firstIndexingBinder)
          ++ genTake(binder, expectedLength)
          ++ lastTrees
        ).toList
      }

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      protected def subPatRefs(binder: Symbol): List[Tree] = {
        if (totalArity > 0 && isSeq) subPatRefsSeq(binder)
        else productElemsToN(binder, totalArity)
      }

      private def compareInts(t1: Tree, t2: Tree) =
        gen.mkMethodCall(termMember(ScalaPackage, "math"), TermName("signum"), Nil, (t1 INT_- t2) :: Nil)

      protected def lengthGuard(binder: Symbol): Option[Tree] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        checkedLength map { expectedLength =>
          // `binder.lengthCompare(expectedLength)`
          // ...if binder has a lengthCompare method, otherwise
          // `scala.math.signum(binder.length - expectedLength)`
          def checkExpectedLength = {
            val tree = seqTree(binder, forceImmutable = false)
            val typedTree = typer.typed(tree)
            val lengthCompareSym = typedTree.tpe.member(nme.lengthCompare)
            if (lengthCompareSym == NoSymbol) compareInts(Select(typedTree, nme.length), LIT(expectedLength))
            else (typedTree DOT lengthCompareSym)(LIT(expectedLength))
          }

          // the comparison to perform
          // when the last subpattern is a wildcard-star the expectedLength is but a lower bound
          // (otherwise equality is required)
          def compareOp: (Tree, Tree) => Tree =
            if (isStar) _ INT_>= _
            else         _ INT_== _

          // `if (binder != null && $checkExpectedLength [== | >=] 0) then else zero`
          (seqTree(binder, forceImmutable = false) ANY_!= NULL) AND compareOp(checkExpectedLength, ZERO)
        }

      def checkedLength: Option[Int] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        if (!isSeq || expectedLength < starArity) None
        else Some(expectedLength)
    }

    // TODO: to be called when there's a def unapplyProd(x: T): U
    // U must have N members _1,..., _N -- the _i are type checked, call their type Ti,
    // for now only used for case classes -- pretending there's an unapplyProd that's the identity (and don't call it)
    class ExtractorCallProd(fun: Tree, args: List[Tree]) extends ExtractorCall(fun, args) {
      /** Create the TreeMaker that embodies this extractor call
       *
       * `binder` has been casted to `paramType` if necessary
       */
      def treeMakers(binder: Symbol, pos: Position): List[TreeMaker] = {
        val paramAccessors = expectedExtractedType.typeSymbol.constrParamAccessors
        val numParams = paramAccessors.length
        def paramAccessorAt(subPatIndex: Int) = paramAccessors(math.min(subPatIndex, numParams - 1))
        // binders corresponding to mutable fields should be stored (scala/bug#5158, scala/bug#6070)
        // make an exception for classes under the scala package as they should be well-behaved,
        // to optimize matching on List
        val mutableBinders = (
          if (!binder.info.typeSymbol.hasTransOwner(ScalaPackageClass) &&
              (paramAccessors exists (x => x.isMutable || definitions.isRepeated(x)))) {

            subPatBinders.zipWithIndex.flatMap {
              case (binder, idx) =>
                val param = paramAccessorAt(idx)
                if (param.isMutable || (definitions.isRepeated(param) && !isStar)) binder :: Nil
                else Nil
            }
          } else Nil
        )

        // checks binder ne null before chaining to the next extractor
        ProductExtractorTreeMaker(binder, lengthGuard(binder))(subPatBinders, subPatRefs(binder), mutableBinders, ignoredSubPatBinders) :: Nil
      }

      // reference the (i-1)th case accessor if it exists, otherwise the (i-1)th tuple component
      override protected def tupleSel(binder: Symbol)(i: Int): Tree = {
        val accessors = expectedExtractedType.typeSymbol.caseFieldAccessors
        if (accessors isDefinedAt (i-1)) gen.mkAttributedStableRef(binder) DOT accessors(i-1)
        else codegen.tupleSel(binder)(i) // this won't type check for case classes, as they do not inherit ProductN
      }
    }

    /**
      *
      * @param fun reference to the unapply method
      * @param args the subpatterns
      * @param unapplyAppliedToDummy an application of the unapply method to the (dummy) unapply selector
      */
    class ExtractorCallRegular(fun: Tree, args: List[Tree])(unapplyAppliedToDummy: Tree) extends ExtractorCall(fun, args) {
      override lazy val unapplySelector =
        unapplyAppliedToDummy match {
          case Apply(_, (dummy@Ident(nme.SELECTOR_DUMMY)) :: Nil) => dummy.symbol
          case _ => NoSymbol // if the unapply is applied to <unapply-selector>.toXXXX, we can't use the selector dummy's symbol
        }

      /** Create the TreeMaker that embodies this extractor call
       *
       *  `binder` has been casted to `paramType` if necessary
       *
       *  TODO: implement review feedback by @retronym:
       *    Passing the pair of values around suggests:
       *       case class Binder(sym: Symbol, knownNotNull: Boolean).
       *    Perhaps it hasn't reached critical mass, but it would already clean things up a touch.
       */
      def treeMakers(patBinderOrCasted: Symbol, pos: Position): List[TreeMaker] = {
        // the extractor call (applied to the binder bound by the flatMap corresponding
        // to the previous (i.e., enclosing/outer) pattern)
        val (extractorApply, needsSubst) = spliceApply(pos, patBinderOrCasted)
        // can't simplify this when subPatBinders.isEmpty, since UnitTpe is definitely
        // wrong when isSeq, and resultInMonad should always be correct since it comes
        // directly from the extractor's result type
        val binder         = freshSym(pos, resultInMonad(patBinderOrCasted))
        val potentiallyMutableBinders: Set[Symbol] =
          if (extractorApply.tpe.typeSymbol.isNonBottomSubClass(OptionClass) && !isSeq)
            Set.empty
          else
            // Ensures we capture unstable bound variables eagerly. These can arise under name based patmat or by indexing into mutable Seqs. See run t9003.scala
            subPatBinders.toSet

        // types may refer to the dummy symbol unapplySelector (in case of dependent method type for the unapply method)
        val extractorTreeMaker = ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder)(
          subPatBinders,
          subPatRefs(binder),
          potentiallyMutableBinders,
          isBool,
          checkedLength,
          patBinderOrCasted,
          ignoredSubPatBinders
        )
        if (needsSubst)
          SubstOnlyTreeMaker(unapplySelector, patBinderOrCasted) :: extractorTreeMaker :: Nil
        else
          extractorTreeMaker :: Nil
      }

      override protected def seqTree(binder: Symbol, forceImmutable: Boolean): Tree =
        if (firstIndexingBinder == 0) {
          val ref = REF(binder)
          if (forceImmutable && !binder.tpe.typeSymbol.isNonBottomSubClass(SeqClass)) Select(ref, nme.toSeq)
          else ref
        }
        else super.seqTree(binder, forceImmutable)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (totalArity > 0 && (!lastIsStar || isSeq))
      override protected def subPatRefs(binder: Symbol): List[Tree] =
        if (isSingle) REF(binder) :: Nil // special case for extractors
        else super.subPatRefs(binder)

      protected def spliceApply(pos: Position, binder: Symbol): (Tree, Boolean) = {
        var needsSubst = false
        object splice extends AstTransformer {
          def binderRef(pos: Position): Tree =
            REF(binder) setPos pos
          override def transform(t: Tree) = t match {
            // duplicated with the extractor Unapplied
            case Apply(x, List(i @ Ident(nme.SELECTOR_DUMMY))) =>
              // in case the result type depended on the unapply's argument, plug in the new symbol
              val apply = treeCopy.Apply(t, x, binderRef(i.pos) :: Nil)
              val tpe = apply.tpe
              val substedTpe = tpe.substSym(List(i.symbol), List(binder))
              if (tpe ne substedTpe) {
                needsSubst = true
                apply.setType(substedTpe)
              }
              apply
            // scala/bug#7868 Account for numeric widening, e.g. <unapplySelector>.toInt
            case Apply(x, List(i @ (sel @ Select(Ident(nme.SELECTOR_DUMMY), name)))) =>
              // not substituting `binder` for `i.symbol`: widening conversion implies the binder could not be used as a path
              treeCopy.Apply(t, x, treeCopy.Select(sel, binderRef(i.pos), name) :: Nil)
            case _ =>
              super.transform(t)
          }
        }
        (atPos(pos)(splice transform unapplyAppliedToDummy), needsSubst)
      }

    }
  }
}
