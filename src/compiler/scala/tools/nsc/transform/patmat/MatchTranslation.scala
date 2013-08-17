/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.language.postfixOps
import scala.collection.mutable
import scala.reflect.internal.util.Statistics


/** Segregating this super hacky code. */
trait CpsPatternHacks {
  self: PatternMatching =>

  import global._

  // duplicated from CPSUtils (avoid dependency from compiler -> cps plugin...)
  private object CpsSymbols {
    val MarkerCPSAdaptPlus  = rootMirror.getClassIfDefined("scala.util.continuations.cpsPlus")
    val MarkerCPSAdaptMinus = rootMirror.getClassIfDefined("scala.util.continuations.cpsMinus")
    val MarkerCPSSynth      = rootMirror.getClassIfDefined("scala.util.continuations.cpsSynth")
    val MarkerCPSTypes      = rootMirror.getClassIfDefined("scala.util.continuations.cpsParam")
    val stripTriggerCPSAnns = Set[Symbol](MarkerCPSSynth, MarkerCPSAdaptMinus, MarkerCPSAdaptPlus)
    val strippedCPSAnns     = stripTriggerCPSAnns + MarkerCPSTypes

    // when one of the internal cps-type-state annotations is present, strip all CPS annotations
    // a cps-type-state-annotated type makes no sense as an expected type (matchX.tpe is used as pt in translateMatch)
    // (only test availability of MarkerCPSAdaptPlus assuming they are either all available or none of them are)
    def removeCPSFromPt(pt: Type): Type = (
      if (MarkerCPSAdaptPlus.exists && (stripTriggerCPSAnns exists pt.hasAnnotation))
        pt filterAnnotations (ann => !(strippedCPSAnns exists ann.matches))
      else
        pt
    )
  }
  def removeCPSFromPt(pt: Type): Type = CpsSymbols removeCPSFromPt pt
}

/** Translate typed Trees that represent pattern matches into the patternmatching IR, defined by TreeMakers.
 */
trait MatchTranslation extends CpsPatternHacks {
  self: PatternMatching =>

  import PatternMatchingStats._
  import global._
  import definitions._
  import global.analyzer.{ErrorUtils, formalTypes}
  import treeInfo.{ Unapplied, isStar }

  // Always map repeated params to sequences
  private def setVarInfo(sym: Symbol, info: Type) =
    sym setInfo debug.patmatResult(s"changing ${sym.defString} to")(repeatedToSeq(info))

  trait MatchTranslator extends TreeMakers {
    import typer.context

    // a list of TreeMakers that encode `patTree`, and a list of arguments for recursive invocations of `translatePattern` to encode its subpatterns
    private case class TranslationStep(makers: List[TreeMaker], subpatterns: List[(Symbol, Tree)]) {
      def merge(f: (Symbol, Tree) => List[TreeMaker]): List[TreeMaker] = makers ::: (subpatterns flatMap f.tupled)
    }

    // Why is it so difficult to say "here's a name and a context, give me any
    // matching symbol in scope" ? I am sure this code is wrong, but attempts to
    // use the scopes of the contexts in the enclosing context chain discover
    // nothing. How to associate a name with a symbol would would be a wonderful
    // linkage for which to establish a canonical acquisition mechanism.
    def matchingSymbolInScope(pat: Tree): Symbol = {
      def declarationOfName(tpe: Type, name: Name): Symbol = tpe match {
        case PolyType(tparams, restpe)  => tparams find (_.name == name) getOrElse declarationOfName(restpe, name)
        case MethodType(params, restpe) => params find (_.name == name) getOrElse declarationOfName(restpe, name)
        case ClassInfoType(_, _, clazz) => clazz.rawInfo member name
        case _                          => NoSymbol
      }
      pat match {
        case Bind(name, _) =>
          context.enclosingContextChain.foldLeft(NoSymbol: Symbol)((res, ctx) =>
            res orElse declarationOfName(ctx.owner.rawInfo, name))
        case _ => NoSymbol
      }
    }

    // Issue better warnings than "unreachable code" when people mis-use
    // variable patterns thinking they bind to existing identifiers.
    //
    // Possible TODO: more deeply nested variable patterns, like
    //   case (a, b) => 1 ; case (c, d) => 2
    // However this is a pain (at least the way I'm going about it)
    // and I have to think these detailed errors are primarily useful
    // for beginners, not people writing nested pattern matches.
    def checkMatchVariablePatterns(cases: List[CaseDef]) {
      // A string describing the first variable pattern
      var vpat: String = null
      // Using an iterator so we can recognize the last case
      val it = cases.iterator

      def addendum(pat: Tree) = {
        matchingSymbolInScope(pat) match {
          case NoSymbol   => ""
          case sym        =>
            val desc = if (sym.isParameter) s"parameter ${sym.nameString} of" else sym + " in"
            s"\nIf you intended to match against $desc ${sym.owner}, you must use backticks, like: case `${sym.nameString}` =>"
        }
      }

      while (it.hasNext) {
        val cdef = it.next()
        // If a default case has been seen, then every succeeding case is unreachable.
        if (vpat != null)
          context.unit./*error*/warning(cdef.body.pos, "unreachable code due to " + vpat + addendum(cdef.pat))
        // If this is a default case and more cases follow, warn about this one so
        // we have a reason to mention its pattern variable name and any corresponding
        // symbol in scope.  Errors will follow from the remaining cases, at least
        // once we make the above warning an error.
        else if (it.hasNext && (treeInfo isDefaultCase cdef)) {
          val vpatName = cdef.pat match {
            case Bind(name, _)   => s" '$name'"
            case _               => ""
          }
          vpat = s"variable pattern$vpatName on line ${cdef.pat.pos.line}"
          context.unit.warning(cdef.pos, s"patterns after a variable pattern cannot match (SLS 8.1.1)" + addendum(cdef.pat))
        }
      }
    }

    /** Implement a pattern match by turning its cases (including the implicit failure case)
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape
      * `runOrElse(scrutinee)(x => translateCase1(x).orElse(translateCase2(x)).....orElse(zero))`
      *
      * NOTE: the resulting tree is not type checked, nor are nested pattern matches transformed
      *   thus, you must typecheck the result (and that will in turn translate nested matches)
      *   this could probably optimized... (but note that the matchStrategy must be solved for each nested patternmatch)
      */
    def translateMatch(match_ : Match): Tree = {
      val Match(selector, cases) = match_

      val (nonSyntheticCases, defaultOverride) = cases match {
        case init :+ last if treeInfo isSyntheticDefaultCase last => (init, Some(((scrut: Tree) => last.body)))
        case _                                                    => (cases, None)
      }

      checkMatchVariablePatterns(nonSyntheticCases)

      // we don't transform after uncurry
      // (that would require more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      if (phase.id >= currentRun.uncurryPhase.id)
        devWarning(s"running translateMatch past uncurry (at $phase) on $selector match $cases")

      debug.patmat("translating "+ cases.mkString("{", "\n", "}"))

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatNanos) else null

      val selectorTp = repeatedToSeq(elimAnonymousClass(selector.tpe.widen.withoutAnnotations))

      // when one of the internal cps-type-state annotations is present, strip all CPS annotations
      val origPt  = removeCPSFromPt(match_.tpe)
      // relevant test cases: pos/existentials-harmful.scala, pos/gadt-gilles.scala, pos/t2683.scala, pos/virtpatmat_exist4.scala
      // pt is the skolemized version
      val pt = repeatedToSeq(origPt)

      // val packedPt = repeatedToSeq(typer.packedType(match_, context.owner))
      val selectorSym = freshSym(selector.pos, pureType(selectorTp)) setFlag treeInfo.SYNTH_CASE_FLAGS

      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      val combined = combineCases(selector, selectorSym, nonSyntheticCases map translateCase(selectorSym, pt), pt, matchOwner, defaultOverride)

      if (Statistics.canEnable) Statistics.stopTimer(patmatNanos, start)
      combined
    }

    // return list of typed CaseDefs that are supported by the backend (typed/bind/wildcard)
    // we don't have a global scrutinee -- the caught exception must be bound in each of the casedefs
    // there's no need to check the scrutinee for null -- "throw null" becomes "throw new NullPointerException"
    // try to simplify to a type-based switch, or fall back to a catch-all case that runs a normal pattern match
    // unlike translateMatch, we type our result before returning it
    def translateTry(caseDefs: List[CaseDef], pt: Type, pos: Position): List[CaseDef] =
      // if they're already simple enough to be handled by the back-end, we're done
      if (caseDefs forall treeInfo.isCatchCase) caseDefs
      else {
        val swatches = { // switch-catches
          val bindersAndCases = caseDefs map { caseDef =>
            // generate a fresh symbol for each case, hoping we'll end up emitting a type-switch (we don't have a global scrut there)
            // if we fail to emit a fine-grained switch, have to do translateCase again with a single scrutSym (TODO: uniformize substitution on treemakers so we can avoid this)
            val caseScrutSym = freshSym(pos, pureType(ThrowableTpe))
            (caseScrutSym, propagateSubstitution(translateCase(caseScrutSym, pt)(caseDef), EmptySubstitution))
          }

          for(cases <- emitTypeSwitch(bindersAndCases, pt).toList
              if cases forall treeInfo.isCatchCase; // must check again, since it's not guaranteed -- TODO: can we eliminate this? e.g., a type test could test for a trait or a non-trivial prefix, which are not handled by the back-end
              cse <- cases) yield fixerUpper(matchOwner, pos)(cse).asInstanceOf[CaseDef]
        }

        val catches = if (swatches.nonEmpty) swatches else {
          val scrutSym = freshSym(pos, pureType(ThrowableTpe))
          val casesNoSubstOnly = caseDefs map { caseDef => (propagateSubstitution(translateCase(scrutSym, pt)(caseDef), EmptySubstitution))}

          val exSym = freshSym(pos, pureType(ThrowableTpe), "ex")

          List(
              atPos(pos) {
                CaseDef(
                  Bind(exSym, Ident(nme.WILDCARD)), // TODO: does this need fixing upping?
                  EmptyTree,
                  combineCasesNoSubstOnly(CODE.REF(exSym), scrutSym, casesNoSubstOnly, pt, matchOwner, Some(scrut => Throw(CODE.REF(exSym))))
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
    def translateCase(scrutSym: Symbol, pt: Type)(caseDef: CaseDef) = {
      val CaseDef(pattern, guard, body) = caseDef
      translatePattern(scrutSym, pattern) ++ translateGuard(guard) :+ translateBody(body, pt)
    }

    def translatePattern(patBinder: Symbol, patTree: Tree): List[TreeMaker] = {
      val pos       = patTree.pos
      def patType   = patBinder.info.dealiasWiden // the type of the variable bound to the pattern

      def glbWithBinder(other: Type) = glb(patType :: other :: Nil).normalize

      def withSubPats(treeMakers: List[TreeMaker], subpats: (Symbol, Tree)*): TranslationStep = TranslationStep(treeMakers, subpats.toList)
      def noFurtherSubPats(treeMakers: TreeMaker*): TranslationStep = TranslationStep(treeMakers.toList, Nil)

      def translateExtractorPattern(extractor: ExtractorCall): TranslationStep = {
        import extractor.paramType  // the type expected by the unapply

        def patConforms = patType <:< paramType
        def patEquiv    = patType =:= paramType

        if (!extractor.isTyped)
          ErrorUtils.issueNormalTypeError(patTree, "Could not typecheck extractor call: "+ extractor)(context)

        debug.patmat("translateExtractorPattern checking parameter type: " + ((patBinder, patType, paramType, patConforms)))

        // example check: List[Int] <:< ::[Int]
        // TODO: extractor.paramType may contain unbound type params (run/t2800, run/t3530)
        // `patBinderOrCasted` is assigned the result of casting `patBinder` to `extractor.paramType`
        val (typeTestTreeMaker, patBinderOrCasted, binderKnownNonNull) =
          if (patConforms) {
            // no type test needed, but the tree maker relies on `patBinderOrCasted` having type `extractor.paramType` (and not just some type compatible with it)
            // SI-6624 shows this is necessary because apparently patBinder may have an unfortunate type (.decls don't have the case field accessors)
            // TODO: get to the bottom of this -- I assume it happens when type checking infers a weird type for an unapply call
            // by going back to the parameterType for the extractor call we get a saner type, so let's just do that for now
            if (!patEquiv)
              devWarning(s"resetting info of $patBinder: $patType to $paramType")

            (Nil, setVarInfo(patBinder, paramType), false)
          }
          else {
            // chain a type-testing extractor before the actual extractor call
            // it tests the type, checks the outer pointer and casts to the expected type
            // TODO: the outer check is mandated by the spec for case classes, but we do it for user-defined unapplies as well [SPEC]
            // (the prefix of the argument passed to the unapply must equal the prefix of the type of the binder)
            val treeMaker = TypeTestTreeMaker(patBinder, patBinder, paramType, paramType)(pos, extractorArgTypeTest = true)

            // check whether typetest implies patBinder is not null,
            // even though the eventual null check will be on patBinderOrCasted
            // it'll be equal to patBinder casted to paramType anyway (and the type test is on patBinder)
            (List(treeMaker), treeMaker.nextBinder, treeMaker.impliesBinderNonNull(patBinder))
          }

        withSubPats(typeTestTreeMaker :+ extractor.treeMaker(patBinderOrCasted, binderKnownNonNull, pos), extractor.subBindersAndPatterns: _*)
      }


      object MaybeBoundTyped {
        object NonNullTyped {
          // the Ident subpattern can be ignored, subpatBinder or patBinder tell us all we need to know about it
          def unapply(tree: Typed): Option[Type] = tree match {
            case Typed(Ident(_), _) if tree.tpe != null => Some((tree.tpe))
            case _                                      => None
          }
        }

        /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
          * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these).
          * The returned type is the one inferred by inferTypedPattern (`owntype`)
          *
          * @arg patBinder  symbol used to refer to the result of the previous pattern's extractor
          *      (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
        def unapply(tree: Tree): Option[(Symbol, Type)] = tree match {
          case Bound(binder, MaybeBoundTyped(_, tpe)) => Some((binder, tpe))     // possible nested bindings - use the outermost
          case NonNullTyped(tpe)                      => Some((patBinder, tpe))  // patBinder used if no local bindings
          case Bind(_, expr)                          => unapply(expr)
          case _                                      => None
        }
      }

      def unsupportedPatternMsg = sm"""
        |unsupported pattern: ${patTree.shortClass} $patTree (this is a scalac bug.)
        |Tree diagnostics:
        |  ${asCompactDebugString(patTree)}
        |""".trim

      def one(maker: TreeMaker) = noFurtherSubPats(maker)
      def none()                = noFurtherSubPats()

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
      val translationStep = patTree match {
        case WildcardPattern()                                        => none()
        case _: UnApply | _: Apply                                    => translateExtractorPattern(ExtractorCall(patTree))
        case MaybeBoundTyped(subPatBinder, pt)                        => one(TypeTestTreeMaker(subPatBinder, patBinder, pt, glbWithBinder(pt))(pos))
        case Bound(subpatBinder, p)                                   => withSubPats(List(SubstOnlyTreeMaker(subpatBinder, patBinder)), (patBinder, p))
        case Literal(Constant(_)) | Ident(_) | Select(_, _) | This(_) => one(EqualityTestTreeMaker(patBinder, patTree, pos))
        case Alternative(alts)                                        => one(AlternativesTreeMaker(patBinder, alts map (translatePattern(patBinder, _)), alts.head.pos))
        case Bind(_, _)                                               => devWarning(s"Bind tree with unbound symbol $patTree") ; none()
        case _                                                        => context.unit.error(patTree.pos, unsupportedPatternMsg) ; none()
      }

      translationStep merge translatePattern
    }

    def translateGuard(guard: Tree): List[TreeMaker] =
      if (guard == EmptyTree) Nil
      else List(GuardTreeMaker(guard))

    // TODO: 1) if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (which is now done by codegen.one),
    // so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand?
    // to enable this, probably need to move away from Option to a monad specific to pattern-match,
    // so that we can return Option's from a match without ambiguity whether this indicates failure in the monad, or just some result in the monad
    // 2) body.tpe is the type of the body after applying the substitution that represents the solution of GADT type inference
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
       provided the run-time type of v is also an instance of T,  <-- TODO! https://issues.scala-lang.org/browse/SI-1503
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
        case UnApply(unfun, args) => new ExtractorCallRegular(unfun, args) // extractor
        case Apply(fun, args)     => new ExtractorCallProd(fun, args)      // case class
      }
    }

    abstract class ExtractorCall {
      import CODE._

      def fun: Tree
      def args: List[Tree]

      val nbSubPats     = args.length
      val starLength    = if (hasStar) 1 else 0
      val nonStarLength = args.length - starLength

      // everything okay, captain?
      def isTyped: Boolean
      def isSeq: Boolean

      private def hasStar       = nbSubPats > 0 && isStar(args.last)
      private def isNonEmptySeq = nbSubPats > 0 && isSeq

      def isSingle = nbSubPats == 0 && !isSeq

      // to which type should the previous binder be casted?
      def paramType  : Type

      protected def rawSubPatTypes: List[Type]

      /** Create the TreeMaker that embodies this extractor call
       *
       * `binder` has been casted to `paramType` if necessary
       * `binderKnownNonNull` indicates whether the cast implies `binder` cannot be null
       * when `binderKnownNonNull` is `true`, `ProductExtractorTreeMaker` does not do a (redundant) null check on binder
       */
      def treeMaker(binder: Symbol, binderKnownNonNull: Boolean, pos: Position): TreeMaker

      // `subPatBinders` are the variables bound by this pattern in the following patterns
      // subPatBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
      // must set infos to `subPatTypes`, which are provided by extractor's result,
      // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
      // (it will later result in a type test when `tp` is not a subtype of `b.info`)
      // TODO: can we simplify this, together with the Bound case?
      lazy val subPatBinders = {
        val binders = args map { case Bound(b, _) => b ; case p => freshSym(p.pos, prefix = "p") }
        if (binders.length != subPatTypes.length)
          devWarning(s"Mismatched binders and subpatterns: binders=$binders, subPatTypes=$subPatTypes")

        (binders, subPatTypes).zipped map setVarInfo
      }
      lazy val subBindersAndPatterns: List[(Symbol, Tree)] = subPatBinders zip unboundArgs

      private def unboundArgs = args map unBound
      private def unBound(t: Tree): Tree = t match {
        case Bound(_, p) => p
        case _           => t
      }

      // never store these in local variables (for PreserveSubPatBinders)
      lazy val ignoredSubPatBinders: Set[Symbol] = subPatBinders zip args collect { case (b, PatternBoundToUnderscore()) => b } toSet

      // do repeated-parameter expansion to match up with the expected number of arguments (in casu, subpatterns)
      private def nonStarSubPatTypes = formalTypes(rawInit :+ repeatedType, nonStarLength)

      def subPatTypes: List[Type] = (
        if (rawSubPatTypes.isEmpty || !isSeq) rawSubPatTypes
        else if (hasStar) nonStarSubPatTypes :+ rawLast
        else nonStarSubPatTypes
      )

      private def emptySub       = rawSubPatTypes.isEmpty
      private def rawLast        = if (emptySub) NothingTpe else rawSubPatTypes.last
      private def rawInit        = rawSubPatTypes dropRight 1
      protected def sequenceType = if (emptySub) NothingTpe else rawLast
      protected def elementType  = if (emptySub) NothingTpe else unapplySeqElementType(rawLast)
      protected def repeatedType = if (emptySub) NothingTpe else scalaRepeatedType(elementType)

      // rawSubPatTypes.last is the Seq, thus there are `rawSubPatTypes.length - 1` non-seq elements in the tuple
      protected def firstIndexingBinder = rawSubPatTypes.length - 1
      protected def lastIndexingBinder  = nbSubPats - 1 - starLength
      protected def expectedLength      = lastIndexingBinder - firstIndexingBinder + 1

      private def productElemsToN(binder: Symbol, n: Int): List[Tree] = 1 to n map tupleSel(binder) toList
      private def genTake(binder: Symbol, n: Int): List[Tree]         = (0 until n).toList map (codegen index seqTree(binder))
      private def genDrop(binder: Symbol, n: Int): List[Tree]         = codegen.drop(seqTree(binder))(expectedLength) :: Nil

      // codegen.drop(seqTree(binder))(nbIndexingIndices)))).toList
      protected def seqTree(binder: Symbol)                = tupleSel(binder)(firstIndexingBinder + 1)
      protected def tupleSel(binder: Symbol)(i: Int): Tree = codegen.tupleSel(binder)(i)

      // the trees that select the subpatterns on the extractor's result,
      // referenced by `binder`
      protected def subPatRefsSeq(binder: Symbol): List[Tree] = {
        def lastTrees: List[Tree] = (
          if (!hasStar) Nil
          else if (expectedLength == 0) seqTree(binder) :: Nil
          else genDrop(binder, expectedLength)
        )
        // this error-condition has already been checked by checkStarPatOK:
        //   if(isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if(lastIsStar) 1 else 0) == nbSubPats, "(resultInMonad, ts, subPatTypes, subPats)= "+(resultInMonad, ts, subPatTypes, subPats))

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
      protected def subPatRefs(binder: Symbol): List[Tree] =
        if (isNonEmptySeq) subPatRefsSeq(binder) else productElemsToN(binder, nbSubPats)

      private def compareInts(t1: Tree, t2: Tree) =
        gen.mkMethodCall(termMember(ScalaPackage, "math"), TermName("signum"), Nil, (t1 INT_- t2) :: Nil)

      protected def lengthGuard(binder: Symbol): Option[Tree] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        checkedLength map { expectedLength =>
          // `binder.lengthCompare(expectedLength)`
          // ...if binder has a lengthCompare method, otherwise
          // `scala.math.signum(binder.length - expectedLength)`
          def checkExpectedLength = sequenceType member nme.lengthCompare match {
            case NoSymbol => compareInts(Select(seqTree(binder), nme.length), LIT(expectedLength))
            case lencmp   => (seqTree(binder) DOT lencmp)(LIT(expectedLength))
          }

          // the comparison to perform
          // when the last subpattern is a wildcard-star the expectedLength is but a lower bound
          // (otherwise equality is required)
          def compareOp: (Tree, Tree) => Tree =
            if (hasStar) _ INT_>= _
            else         _ INT_== _

          // `if (binder != null && $checkExpectedLength [== | >=] 0) then else zero`
          (seqTree(binder) ANY_!= NULL) AND compareOp(checkExpectedLength, ZERO)
        }

      def checkedLength: Option[Int] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        if (!isSeq || expectedLength < starLength) None
        else Some(expectedLength)
    }

    // TODO: to be called when there's a def unapplyProd(x: T): U
    // U must have N members _1,..., _N -- the _i are type checked, call their type Ti,
    // for now only used for case classes -- pretending there's an unapplyProd that's the identity (and don't call it)
    class ExtractorCallProd(val fun: Tree, val args: List[Tree]) extends ExtractorCall {
      // TODO: fix the illegal type bound in pos/t602 -- type inference messes up before we get here:
      /*override def equals(x$1: Any): Boolean = ...
             val o5: Option[com.mosol.sl.Span[Any]] =  // Span[Any] --> Any is not a legal type argument for Span!
      */

      private def constructorTp = fun.tpe

      def isTyped    = fun.isTyped

      // to which type should the previous binder be casted?
      def paramType  = constructorTp.finalResultType

      def isSeq = isVarArgTypes(rawSubPatTypes)

      protected def rawSubPatTypes = constructorTp.paramTypes

      /** Create the TreeMaker that embodies this extractor call
       *
       * `binder` has been casted to `paramType` if necessary
       * `binderKnownNonNull` indicates whether the cast implies `binder` cannot be null
       * when `binderKnownNonNull` is `true`, `ProductExtractorTreeMaker` does not do a (redundant) null check on binder
       */
      def treeMaker(binder: Symbol, binderKnownNonNull: Boolean, pos: Position): TreeMaker = {
        val paramAccessors = binder.constrParamAccessors
        // binders corresponding to mutable fields should be stored (SI-5158, SI-6070)
        // make an exception for classes under the scala package as they should be well-behaved,
        // to optimize matching on List
        val mutableBinders =
          if (!binder.info.typeSymbol.hasTransOwner(ScalaPackageClass) &&
              (paramAccessors exists (_.isMutable)))
            subPatBinders.zipWithIndex.collect{ case (binder, idx) if paramAccessors(idx).isMutable => binder }
          else Nil

        // checks binder ne null before chaining to the next extractor
        ProductExtractorTreeMaker(binder, lengthGuard(binder))(subPatBinders, subPatRefs(binder), mutableBinders, binderKnownNonNull, ignoredSubPatBinders)
      }

      // reference the (i-1)th case accessor if it exists, otherwise the (i-1)th tuple component
      override protected def tupleSel(binder: Symbol)(i: Int): Tree = { import CODE._
        val accessors = binder.caseFieldAccessors
        if (accessors isDefinedAt (i-1)) REF(binder) DOT accessors(i-1)
        else codegen.tupleSel(binder)(i) // this won't type check for case classes, as they do not inherit ProductN
      }

      override def toString() = s"ExtractorCallProd($fun:${fun.tpe} / ${fun.symbol} / args=$args)"
    }

    class ExtractorCallRegular(extractorCallIncludingDummy: Tree, val args: List[Tree]) extends ExtractorCall {
      val Unapplied(fun) = extractorCallIncludingDummy

      def tpe        = fun.tpe
      def paramType  = firstParamType(tpe)
      def resultType = fun.tpe.finalResultType
      def isTyped    = (tpe ne NoType) && fun.isTyped && (resultInMonad ne ErrorType)
      def isSeq      = fun.symbol.name == nme.unapplySeq
      def isBool     = resultType =:= BooleanTpe

      /** Create the TreeMaker that embodies this extractor call
       *
       *  `binder` has been casted to `paramType` if necessary
       *  `binderKnownNonNull` is not used in this subclass
       *
       *  TODO: implement review feedback by @retronym:
       *    Passing the pair of values around suggests:
       *       case class Binder(sym: Symbol, knownNotNull: Boolean).
       *    Perhaps it hasn't reached critical mass, but it would already clean things up a touch.
       */
      def treeMaker(patBinderOrCasted: Symbol, binderKnownNonNull: Boolean, pos: Position): TreeMaker = {
        // the extractor call (applied to the binder bound by the flatMap corresponding
        // to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = atPos(pos)(spliceApply(patBinderOrCasted))
        // can't simplify this when subPatBinders.isEmpty, since UnitTpe is definitely
        // wrong when isSeq, and resultInMonad should always be correct since it comes
        // directly from the extractor's result type
        val binder         = freshSym(pos, pureType(resultInMonad))

        ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder)(
          subPatBinders,
          subPatRefs(binder),
          isBool,
          checkedLength,
          patBinderOrCasted,
          ignoredSubPatBinders
        )
      }

      override protected def seqTree(binder: Symbol): Tree =
        if (firstIndexingBinder == 0) CODE.REF(binder)
        else super.seqTree(binder)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      override protected def subPatRefs(binder: Symbol): List[Tree] =
        if (!isSeq && nbSubPats == 1) List(CODE.REF(binder)) // special case for extractors
        else super.subPatRefs(binder)

      protected def spliceApply(binder: Symbol): Tree = {
        object splice extends Transformer {
          override def transform(t: Tree) = t match {
            case Apply(x, List(i @ Ident(nme.SELECTOR_DUMMY))) =>
              treeCopy.Apply(t, x, List(CODE.REF(binder) setPos i.pos))
            case _ =>
              super.transform(t)
          }
        }
        splice transform extractorCallIncludingDummy
      }

      // what's the extractor's result type in the monad?
      // turn an extractor's result type into something `monadTypeToSubPatTypesAndRefs` understands
      protected lazy val resultInMonad: Type = if (isBool) UnitTpe else matchMonadResult(resultType) // the type of "get"

      protected lazy val rawSubPatTypes = (
        if (isBool) Nil
        else if (!isSeq && nbSubPats == 1) resultInMonad :: Nil
        else getNameBasedProductSelectorTypes(resultInMonad) match {
          case Nil => resultInMonad :: Nil
          case x   => x
        }
      )

      override def toString() = s"ExtractorCallRegular($fun:${fun.tpe} / ${fun.symbol})"
    }

    /** A conservative approximation of which patterns do not discern anything.
     * They are discarded during the translation.
     */
    object WildcardPattern {
      def unapply(pat: Tree): Boolean = pat match {
        case Bind(nme.WILDCARD, WildcardPattern()) => true // don't skip when binding an interesting symbol!
        case Ident(nme.WILDCARD)                   => true
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

    object Bound {
      def unapply(t: Tree): Option[(Symbol, Tree)] = t match {
        case t@Bind(n, p) if (t.symbol ne null) && (t.symbol ne NoSymbol) => // pos/t2429 does not satisfy these conditions
          Some((t.symbol, p))
        case _ => None
      }
    }
  }
}
