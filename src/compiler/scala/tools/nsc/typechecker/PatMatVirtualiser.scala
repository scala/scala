/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package typechecker

import symtab._
import Flags.{ CASE => _, _ }


/** Translate pattern matching into method calls (these methods form a zero-plus monad), similar in spirit to how for-comprehensions are compiled.
  *
  * For each case, express all patterns as extractor calls, guards as 0-ary extractors, and sequence them using `flatMap`
  * (lifting the body of the case into the monad using `one`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `zero`).
  *
  * The monad `M` in which the pattern match is interpreted is determined by solving `implicitly[MatchingStrategy[M]]` for M.
  * Predef provides the default, `OptionMatching`

  * Example translation: TODO

    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    (a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body)))))(scrut)

TODO:
 - optimizer loops on virtpatmat compiler?

 - don't orElse a failure case at the end if there's a default case
 - implement spec more closely (see TODO's below)
 - fix inlining of methods in nested objects

  * (longer-term) TODO:
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  *  - recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  */
trait PatMatVirtualiser extends ast.TreeDSL { self: Analyzer =>
  import global._
  import definitions._

  private lazy val matchingStrategyTycon = definitions.getClass("scala.MatchingStrategy").typeConstructor

  class MatchTranslator(typer: Typer) extends MatchCodeGen {
    def typed(tree: Tree, mode: Int, pt: Type): Tree = typer.typed(tree, mode, pt) // for MatchCodeGen -- imports don't provide implementations for abstract members

    import typer._
    import typeDebug.{ ptTree, ptBlock, ptLine }

    def solveContextBound(contextBoundTp: Type): (Tree, Type) = {
      val solSym      = NoSymbol.newTypeParameter(NoPosition, "SolveImplicit$".toTypeName)
      val param       = solSym.setInfo(contextBoundTp.typeSymbol.typeParams(0).info.cloneInfo(solSym)) // TypeBounds(NothingClass.typeConstructor, baseTp)
      val pt          = appliedType(contextBoundTp, List(param.tpeHK))
      val savedUndets = context.undetparams

      context.undetparams = param :: context.undetparams
      val result          = inferImplicit(EmptyTree, pt, false, false, context)
      context.undetparams = savedUndets

      (result.tree, result.subst.to(result.subst.from indexOf param))
    }

    lazy val (matchingStrategy, matchingMonadType) = solveContextBound(matchingStrategyTycon)

    /** Implement a pattern match by turning its cases (including the implicit failure case)
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape
      * `runOrElse(scrutinee)(x => translateCase1(x).orElse(translateCase2(x)).....orElse(zero))`
      *
      * NOTE: the resulting tree is not type checked, nor are nested pattern matches transformed
      *   thus, you must typecheck the result (and that will in turn translate nested matches)
      *   this could probably optimized... (but note that the matchingStrategy must be solved for each nested patternmatch)
      */
    def translateMatch(scrut: Tree, cases: List[CaseDef], pt: Type): Tree = {
      // we don't transform after typers
      // (that would require much more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      assert(phase.id <= currentRun.typerPhase.id)

      val scrutType = repeatedToSeq(elimAnonymousClass(scrut.tpe.widen))

      val scrutSym  = freshSym(scrut.pos, scrutType)

      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      fixerUpper(context.owner, scrut.pos)(combineCases(scrut, scrutSym, cases map translateCase(scrutSym), repeatedToSeq(pt)))
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
    def translateCase(scrutSym: Symbol)(caseDef: CaseDef) = caseDef match { case CaseDef(pattern, guard, body) =>
      (translatePattern(scrutSym, pattern) ++ translateGuard(guard), translateBody(body))
    }

    def translatePattern(patBinder: Symbol, patTree: Tree): List[TreeMaker] = {
      // a list of TreeMakers that encode `patTree`, and a list of arguments for recursive invocations of `translatePattern` to encode its subpatterns
      type TranslationStep = (List[TreeMaker], List[(Symbol, Tree)])
      @inline def withSubPats(treeMakers: List[TreeMaker], subpats: (Symbol, Tree)*): TranslationStep = (treeMakers, subpats.toList)
      @inline def noFurtherSubPats(treeMakers: TreeMaker*): TranslationStep = (treeMakers.toList, Nil)

      val pos = patTree.pos

      def translateExtractorPattern(extractor: ExtractorCall): TranslationStep = {
        if (!extractor.isTyped) throw new TypeError(pos, "Could not typecheck extractor call: "+ extractor)
        if (extractor.resultInMonad == ErrorType) throw new TypeError(pos, "Unsupported extractor type: "+ extractor.tpe)

        // must use type `tp`, which is provided by extractor's result, not the type expected by binder,
        // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
        // (it will later result in a type test when `tp` is not a subtype of `b.info`)
        (extractor.subPatBinders, extractor.subPatTypes).zipped foreach { case (b, tp) => b setInfo tp } // println("changing "+ b +" : "+ b.info +" -> "+ tp);

        // println("translateExtractorPattern checking parameter type: "+ (patBinder, patBinder.info.widen, extractor.paramType, patBinder.info.widen <:< extractor.paramType))
        // example check: List[Int] <:< ::[Int]
        // TODO: extractor.paramType may contain unbound type params (run/t2800, run/t3530)
        val (typeTestTreeMaker, patBinderOrCasted) =
          if (needsTypeTest(patBinder.info.widen, extractor.paramType)) {
            // chain a type-testing extractor before the actual extractor call
            // it tests the type, checks the outer pointer and casts to the expected type
            // TODO: the outer check is mandated by the spec for case classes, but we do it for user-defined unapplies as well [SPEC]
            // (the prefix of the argument passed to the unapply must equal the prefix of the type of the binder)
            val treeMaker = TypeTestTreeMaker(patBinder, extractor.paramType, pos)
            (List(treeMaker), treeMaker.nextBinder)
          } else (Nil, patBinder)

        withSubPats(typeTestTreeMaker :+ extractor.treeMaker(patBinderOrCasted, pos), extractor.subBindersAndPatterns: _*)
      }

      /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
        * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these)
        *
        * @arg patBinder  symbol used to refer to the result of the previous pattern's extractor (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
      object MaybeBoundTyped {
        // the returned type is the one inferred by inferTypedPattern (`owntype`)
        def unapply(tree: Tree): Option[(Symbol, Type)] = tree match {
          case Bound(subpatBinder, typed@Typed(expr, tpt)) => Some((subpatBinder, typed.tpe))
          case Bind(_, typed@Typed(expr, tpt))             => Some((patBinder, typed.tpe))
          case Typed(expr, tpt)                            => Some((patBinder, tree.tpe))
          case _                                           => None
        }
      }

      val (treeMakers, subpats) = patTree match {
        // skip wildcard trees -- no point in checking them
        case WildcardPattern() => noFurtherSubPats()
        case UnApply(unfun, args) =>
          // TODO: check unargs == args
          // println("unfun: "+ (unfun.tpe, unfun.symbol.ownerChain, unfun.symbol.info, patBinder.info))
          translateExtractorPattern(ExtractorCall(unfun, args))

        /** A constructor pattern is of the form c(p1, ..., pn) where n ≥ 0.
          It consists of a stable identifier c, followed by element patterns p1, ..., pn.
          The constructor c is a simple or qualified name which denotes a case class (§5.3.2).

          If the case class is monomorphic, then it must conform to the expected type of the pattern,
          and the formal parameter types of x’s primary constructor (§5.3) are taken as the expected types of the element patterns p1, ..., pn.

          If the case class is polymorphic, then its type parameters are instantiated so that the instantiation of c conforms to the expected type of the pattern.
          The instantiated formal parameter types of c’s primary constructor are then taken as the expected types of the component patterns p1, ..., pn.

          The pattern matches all objects created from constructor invocations c(v1, ..., vn) where each element pattern pi matches the corresponding value vi .
          A special case arises when c’s formal parameter types end in a repeated parameter. This is further discussed in (§8.1.9).
        **/
        case Apply(fun, args)     =>
          ExtractorCall.fromCaseClass(fun, args) map translateExtractorPattern getOrElse {
            error("cannot find unapply member for "+ fun +" with args "+ args)
            noFurtherSubPats()
          }

        /** A typed pattern x : T consists of a pattern variable x and a type pattern T.
            The type of x is the type pattern T, where each type variable and wildcard is replaced by a fresh, unknown type.
            This pattern matches any value matched by the type pattern T (§8.2); it binds the variable name to that value.
        **/
        // must treat Typed and Bind together -- we need to know the patBinder of the Bind pattern to get at the actual type
        case MaybeBoundTyped(subPatBinder, pt) =>
          // a typed pattern never has any subtrees
          noFurtherSubPats(TypeAndEqualityTestTreeMaker(subPatBinder, patBinder, pt, pos))

        /** A pattern binder x@p consists of a pattern variable x and a pattern p.
            The type of the variable x is the static type T of the pattern p.
            This pattern matches any value v matched by the pattern p,
            provided the run-time type of v is also an instance of T,  <-- TODO! https://issues.scala-lang.org/browse/SI-1503
            and it binds the variable name to that value.
        **/
        case Bound(subpatBinder, p)          =>
          // TreeMaker with empty list of trees only performs the substitution subpatBinder --> patBinder
          // println("rebind "+ subpatBinder +" to "+ patBinder)
          withSubPats(List(SubstOnlyTreeMaker(Substitution(subpatBinder, CODE.REF(patBinder)))),
            // the symbols are markers that may be used to refer to the result of the extractor in which the corresponding tree is nested
            // it's the responsibility of the treemaker to replace this symbol by a reference that
            // selects that result on the function symbol of the flatMap call that binds to the result of this extractor
            // must be patBinder, as subpatBinder has the wrong info: even if the bind assumes a better type, this is not guaranteed until we cast
            (patBinder, p)
          )

        /** 8.1.4 Literal Patterns
              A literal pattern L matches any value that is equal (in terms of ==) to the literal L.
              The type of L must conform to the expected type of the pattern.

            8.1.5 Stable Identifier Patterns  (a stable identifier r (see §3.1))
              The pattern matches any value v such that r == v (§12.1).
              The type of r must conform to the expected type of the pattern.
        **/
        case Literal(Constant(_)) | Ident(_) | Select(_, _) =>
          noFurtherSubPats(EqualityTestTreeMaker(patBinder, patTree, pos))

        case Alternative(alts)    =>
          val altTrees = alts map { alt =>
            // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
            // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
            // `one(x) : T` where x is the binder before this pattern, which will be replaced by the binder for the alternative by TreeMaker.singleBinder below
            // T is the widened type of the previous binder -- this ascription is necessary to infer a clean type for `or` -- the alternative combinator -- in the presence of existential types
            // see pos/virtpatmat_exist1.scala
            combineExtractors(translatePattern(patBinder, alt), pmgen.one(CODE.REF(patBinder), patBinder.info.widen))
          }

          noFurtherSubPats(AlternativesTreeMaker(patBinder, altTrees : _*))

      /* TODO: Paul says about future version: I think this should work, and always intended to implement if I can get away with it.
          case class Foo(x: Int, y: String)
          case class Bar(z: Int)

          def f(x: Any) = x match { case Foo(x, _) | Bar(x) => x } // x is lub of course.
      */

        case Bind(n, p) => // this happens in certain ill-formed programs, there'll be an error later
          // println("WARNING: Bind tree with unbound symbol "+ patTree)
          noFurtherSubPats() // there's no symbol -- something's wrong... don't fail here though (or should we?)

        // case Star(_) | ArrayValue | This => error("stone age pattern relics encountered!")

        case _                       =>
          error("unsupported pattern: "+ patTree +"(a "+ patTree.getClass +")")
          noFurtherSubPats()
      }

      treeMakers ++ subpats.flatMap { case (binder, pat) =>
        translatePattern(binder, pat) // recurse on subpatterns
      }
    }

    def translateGuard(guard: Tree): List[TreeMaker] =
      if (guard == EmptyTree) Nil
      else List(GuardTreeMaker(guard))

    // TODO: 1) if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (which is now done by pmgen.caseResult),
    // so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand?
    // to enable this, probably need to move away from Option to a monad specific to pattern-match,
    // so that we can return Option's from a match without ambiguity whether this indicates failure in the monad, or just some result in the monad
    // 2) body.tpe is the type of the body after applying the substitution that represents the solution of GADT type inference
    // need the explicit cast in case our substitutions in the body change the type to something that doesn't take GADT typing into account
    def translateBody(body: Tree): Tree = atPos(body.pos)(pmgen.caseResult(body, body.tpe))


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// helper methods: they analyze types and trees in isolation, but they are not (directly) concerned with the structure of the overall translation

    object ExtractorCall {
      def apply(unfun: Tree, args: List[Tree]): ExtractorCall = new ExtractorCall(unfun, args)

      // generate a call to the (synthetically generated) extractor of a case class
      // NOTE: it's an apply, not a select, since in general an extractor call may have multiple argument lists (including an implicit one)
      // that we need to preserve, so we supply the scrutinee as Ident(nme.SELECTOR_DUMMY),
      // and replace that dummy by a reference to the actual binder in translateExtractorPattern
      def fromCaseClass(fun: Tree, args: List[Tree]): Option[ExtractorCall] = {
        // TODO: can we rework the typer so we don't have to do all this twice?
        // undo rewrite performed in (5) of adapt
        val orig      = fun match {case tpt: TypeTree => tpt.original case _ => fun}
        val origSym   = orig.symbol
        val extractor = unapplyMember(origSym.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe)

        if((fun.tpe eq null) || fun.tpe.isError || (extractor eq NoSymbol)) {
          None
        } else {
          // this is a tricky balance: pos/t602.scala, pos/sudoku.scala, run/virtpatmat_alts.scala must all be happy
          // bypass typing at own risk: val extractorCall = Select(orig, extractor) setType caseClassApplyToUnapplyTp(fun.tpe)
          // can't always infer type arguments (pos/t602):
          /*  case class Span[K <: Ordered[K]](low: Option[K]) {
                override def equals(x: Any): Boolean = x match {
                  case Span((low0 @ _)) if low0 equals low => true
                }
              }*/
          // so... leave undetermined type params floating around if we have to
          // (if we don't infer types, uninstantiated type params show up later: pos/sudoku.scala)
          // (see also run/virtpatmat_alts.scala)
          val savedUndets = context.undetparams
          val extractorCall = try {
            context.undetparams = Nil
            silent(_.typed(Apply(Select(orig, extractor), List(Ident(nme.SELECTOR_DUMMY) setType fun.tpe.finalResultType)), EXPRmode, WildcardType), reportAmbiguousErrors = false) match {
              case extractorCall: Tree => extractorCall // if !extractorCall.containsError()
              case _ =>
                // this fails to resolve overloading properly...
                // Apply(typedOperator(Select(orig, extractor)), List(Ident(nme.SELECTOR_DUMMY))) // no need to set the type of the dummy arg, it will be replaced anyway

                // println("funtpe after = "+ fun.tpe.finalResultType)
                // println("orig: "+(orig, orig.tpe))
                val tgt = typed(orig, EXPRmode | QUALmode | POLYmode, HasMember(extractor.name)) // can't specify fun.tpe.finalResultType as the type for the extractor's arg,
                // as it may have been inferred incorrectly (see t602, where it's  com.mosol.sl.Span[Any], instead of  com.mosol.sl.Span[?K])
                // println("tgt = "+ (tgt, tgt.tpe))
                val oper = typed(Select(tgt, extractor.name), EXPRmode | FUNmode | POLYmode | TAPPmode, WildcardType)
                // println("oper: "+ (oper, oper.tpe))
                Apply(oper, List(Ident(nme.SELECTOR_DUMMY))) // no need to set the type of the dummy arg, it will be replaced anyway
            }
          } finally context.undetparams = savedUndets

          Some(this(extractorCall, args)) // TODO: simplify spliceApply?
        }
      }
    }

    class ExtractorCall(extractorCallIncludingDummy: Tree, val args: List[Tree]) {
      private lazy val Some(Apply(extractorCall, _)) = extractorCallIncludingDummy.find{ case Apply(_, List(Ident(nme.SELECTOR_DUMMY))) => true case _ => false }

      def tpe        = extractorCall.tpe
      def isTyped    = (tpe ne NoType) && extractorCall.isTyped
      def resultType = tpe.finalResultType
      def paramType  = tpe.paramTypes.head

      // what's the extractor's result type in the monad?
      // turn an extractor's result type into something `monadTypeToSubPatTypesAndRefs` understands
      lazy val resultInMonad: Type = if(!hasLength(tpe.paramTypes, 1)) ErrorType else {
        if (resultType.typeSymbol == BooleanClass) UnitClass.tpe
        else {
          val monadArgs = resultType.baseType(matchingMonadType.typeSymbol).typeArgs
          // assert(monadArgs.length == 1, "unhandled extractor type: "+ extractorTp) // TODO: overloaded unapply??
          if(monadArgs.length == 1) monadArgs(0)
          else ErrorType
        }
      }

      // `subPatBinders` are the variables bound by this pattern in the following patterns
      // subPatBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
      lazy val subPatBinders = args map {
        case Bound(b, p) => b
        case p => freshSym(p.pos, prefix = "p")
      }

      lazy val subBindersAndPatterns: List[(Symbol, Tree)] = (subPatBinders zip args) map {
        case (b, Bound(_, p)) => (b, p)
        case bp => bp
      }

      def isSeq                            = extractorCall.symbol.name == nme.unapplySeq
      lazy val nbSubPats                   = args.length
      lazy val lastIsStar                  = (nbSubPats > 0) && treeInfo.isStar(args.last)

      // the types for the binders corresponding to my subpatterns
      // subPatTypes != args map (_.tpe) since the args may have more specific types than the constructor's parameter types
      // replace last type (of shape Seq[A]) with RepeatedParam[A] so that formalTypes will
      // repeat the last argument type to align the formals with the number of arguments
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      def subPatTypes: List[Type] =
        if(isSeq) {
          val TypeRef(pre, SeqClass, args) = seqTp
          // do repeated-parameter expansion to match up with the expected number of arguments (in casu, subpatterns)
          formalTypes(rawSubPatTypes.init :+ typeRef(pre, RepeatedParamClass, args), nbSubPats)
        } else rawSubPatTypes

      def treeMaker(patBinderOrCasted: Symbol, pos: Position): TreeMaker = {
        // the extractor call (applied to the binder bound by the flatMap corresponding to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = atPos(pos)(spliceApply(patBinderOrCasted))

        val patTreeLifted =
          if (resultType.typeSymbol == BooleanClass) pmgen.cond(extractorApply)
          else extractorApply

        val binder     = freshSym(pos, resultInMonad) // can't simplify this when subPatBinders.isEmpty, since UnitClass.tpe is definitely wrong when isSeq, and resultInMonad should always be correct since it comes directly from the extractor's result type
        val subpatRefs = if (subPatBinders isEmpty) Nil else subPatRefs(binder)

        lengthGuard(binder) match {
          case None           => ExtractorTreeMaker(patTreeLifted, binder, Substitution(subPatBinders, subpatRefs))
          case Some(lenGuard) => FilteredExtractorTreeMaker(patTreeLifted, lenGuard, binder, Substitution(subPatBinders, subpatRefs))
        }
      }

      protected def spliceApply(binder: Symbol): Tree = {
        object splice extends Transformer {
          override def transform(t: Tree) = t match {
            case Apply(x, List(Ident(nme.SELECTOR_DUMMY))) =>
              treeCopy.Apply(t, x, List(CODE.REF(binder)))
            case _ => super.transform(t)
          }
        }
        splice.transform(extractorCallIncludingDummy)
      }

      private lazy val rawSubPatTypes =
        if (resultInMonad.typeSymbol eq UnitClass) Nil
        else if(nbSubPats == 1)                    List(resultInMonad)
        else getProductArgs(resultInMonad) match {
          case Nil => List(resultInMonad)
          case x   => x
        }

      private def seqLenCmp                = rawSubPatTypes.last member nme.lengthCompare
      private def seqTp                    = rawSubPatTypes.last baseType SeqClass
      private lazy val firstIndexingBinder = rawSubPatTypes.length - 1 // rawSubPatTypes.last is the Seq, thus there are `rawSubPatTypes.length - 1` non-seq elements in the tuple
      private lazy val lastIndexingBinder  = if(lastIsStar) nbSubPats-2 else nbSubPats-1
      private lazy val expectedLength      = lastIndexingBinder - firstIndexingBinder + 1
      private lazy val minLenToCheck       = if(lastIsStar) 1 else 0
      private def seqTree(binder: Symbol)  = if(firstIndexingBinder == 0) CODE.REF(binder) else pmgen.tupleSel(binder)(firstIndexingBinder+1)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      private def subPatRefs(binder: Symbol): List[Tree] = {
        // only relevant if isSeq: (here to avoid capturing too much in the returned closure)
        val indexingIndices               = (0 to (lastIndexingBinder-firstIndexingBinder))
        val nbIndexingIndices             = indexingIndices.length

        // this error is checked by checkStarPatOK
        // if(isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if(lastIsStar) 1 else 0) == nbSubPats, "(resultInMonad, ts, subPatTypes, subPats)= "+(resultInMonad, ts, subPatTypes, subPats))

        (if(isSeq) {
          // there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
          ((1 to firstIndexingBinder) map pmgen.tupleSel(binder)) ++
          // then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
          (indexingIndices map pmgen.index(seqTree(binder))) ++
          // the last one -- if the last subpattern is a sequence wildcard: drop the prefix (indexed by the refs on the line above), return the remainder
          (if(!lastIsStar) Nil else List(
            if(nbIndexingIndices == 0) seqTree(binder)
            else pmgen.drop(seqTree(binder))(nbIndexingIndices)))
        }
        else if(nbSubPats == 1) List(CODE.REF(binder))
        else ((1 to nbSubPats) map pmgen.tupleSel(binder))).toList
      }

      private def lengthGuard(binder: Symbol): Option[Tree] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        if (!isSeq || (expectedLength < minLenToCheck)) None
        else { import CODE._
          // `binder.lengthCompare(expectedLength)`
          def checkExpectedLength = (seqTree(binder) DOT seqLenCmp)(LIT(expectedLength))

          // the comparison to perform
          // when the last subpattern is a wildcard-star the expectedLength is but a lower bound
          // (otherwise equality is required)
          def compareOp: (Tree, Tree) => Tree =
            if (lastIsStar)  _ INT_>= _
            else             _ INT_== _

          // `if (binder != null && $checkExpectedLength [== | >=] 0) then else zero`
          Some((seqTree(binder) ANY_!= NULL) AND compareOp(checkExpectedLength, ZERO))
        }

      override def toString() = extractorCall +": "+ extractorCall.tpe +" (symbol= "+ extractorCall.symbol +")."
    }

    // tack an outer test onto `cond` if binder.info and expectedType warrant it
    def maybeWithOuterCheck(binder: Symbol, expectedTp: Type)(cond: Tree): Tree = { import CODE._
      if (   !((expectedTp.prefix eq NoPrefix) || expectedTp.prefix.typeSymbol.isPackageClass)
          && needsOuterTest(expectedTp, binder.info, context.owner)) {
        val expectedPrefix = expectedTp.prefix match {
          case ThisType(clazz)  => THIS(clazz)
          case pre              => REF(pre.prefix, pre.termSymbol)
        }

        // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
        // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
        val outer = expectedTp.typeSymbol.newMethod(vpmName.outer) setInfo expectedTp.prefix setFlag SYNTHETIC
        val outerCheck = (Select(pmgen._asInstanceOf(binder, expectedTp), outer)) OBJ_EQ expectedPrefix

        // first check cond, since that should ensure we're not selecting outer on null
        pmgen.and(cond, outerCheck)
      }
      else
        cond
    }

    // TODO: also need to test when erasing pt loses crucial information (and if we can recover it using a manifest)
    def needsTypeTest(tp: Type, pt: Type) = !(tp <:< pt)
    def typeTest(binder: Symbol, pt: Type) = maybeWithOuterCheck(binder, pt)(pmgen._isInstanceOf(binder, pt))

    /** Type patterns consist of types, type variables, and wildcards. A type pattern T is of one of the following forms:
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

    // generate the tree for the run-time test that follows from the fact that
    // a `scrut` of known type `scrutTp` is expected to have type `expectedTp`
    // uses maybeWithOuterCheck to check the type's prefix
    def typeAndEqualityTest(patBinder: Symbol, pt: Type): Tree = { import CODE._
       // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
       // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne' will always yield false")
      def genEqualsAndInstanceOf(sym: Symbol): Tree
        = pmgen._equals(REF(sym), patBinder) AND pmgen._isInstanceOf(patBinder, pt.widen)

      def isRefTp(tp: Type) = tp <:< AnyRefClass.tpe

      val patBinderTp = patBinder.info.widen
      def isMatchUnlessNull = isRefTp(pt) && !needsTypeTest(patBinderTp, pt)

      // TODO: [SPEC] type test for Array
      // TODO: use manifests to improve tests (for erased types we can do better when we have a manifest)
      pt match {
          case SingleType(_, sym) /*this implies sym.isStable*/ => genEqualsAndInstanceOf(sym) // TODO: [SPEC] the spec requires `eq` instead of `==` here
          case ThisType(sym) if sym.isModule                    => genEqualsAndInstanceOf(sym) // must use == to support e.g. List() == Nil
          case ThisType(sym)                                    => REF(patBinder) OBJ_EQ This(sym)
          case ConstantType(Constant(null)) if isRefTp(patBinderTp) => REF(patBinder) OBJ_EQ NULL
          case ConstantType(const)                              => pmgen._equals(Literal(const), patBinder)
          case _ if isMatchUnlessNull                           => maybeWithOuterCheck(patBinder, pt)(REF(patBinder) OBJ_NE NULL)
          case _                                                => typeTest(patBinder, pt)
        }
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
        case Alternative(ps)                       => ps forall (WildcardPattern.unapply(_))
        case EmptyTree                             => true
        case _                                     => false
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait TreeMakers {
    trait TreeMaker {
      def substitution: Substitution ={
        if (currSub eq null) currSub = initialSubstitution
        currSub
      }

      protected def initialSubstitution: Substitution

      private[TreeMakers] def addOuterSubstitution(outerSubst: Substitution): TreeMaker = {
        currSub = outerSubst >> substitution
        this
      }
      private[this] var currSub: Substitution = null

      def chainBefore(next: Tree): Tree
    }

    case class SubstOnlyTreeMaker(initialSubstitution: Substitution) extends TreeMaker {
      def chainBefore(next: Tree): Tree = substitution(next)
    }

    trait FunTreeMaker extends TreeMaker {
      val nextBinder: Symbol
      // wrap a Fun (with binder nextBinder) around the next tree (unless nextBinder == NoSymbol) and perform our substitution
      protected def wrapFunSubst(next: Tree): Tree = pmgen.fun(nextBinder, substitution(next))
    }

    trait FreshFunTreeMaker extends FunTreeMaker {
      val pos: Position
      val nextBinderTp: Type
      lazy val nextBinder = freshSym(pos, nextBinderTp)
    }

    trait SingleExtractorTreeMaker extends FunTreeMaker {
      val extractor: Tree
      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree): Tree = pmgen.flatMap(extractor, wrapFunSubst(next)) setPos extractor.pos
    }

    trait SingleBinderTreeMaker extends FunTreeMaker {
      val prevBinder: Symbol
      lazy val initialSubstitution = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))
    }

    abstract class SimpleTreeMaker extends SingleExtractorTreeMaker with SingleBinderTreeMaker with FreshFunTreeMaker

    /**
     * Make a TreeMaker that will result in an extractor call specified by `extractor`
     * the next TreeMaker (here, we don't know which it'll be) is chained after this one by flatMap'ing
     * a function with binder `nextBinder` over our extractor's result
     * the function's body is determined by the next TreeMaker
     * in this function's body, and all the subsequent ones, references to the symbols in `from` will be replaced by the corresponding tree in `to`
     */
    case class ExtractorTreeMaker(extractor: Tree, nextBinder: Symbol, initialSubstitution: Substitution) extends SingleExtractorTreeMaker

    case class FilteredExtractorTreeMaker(extractor: Tree, guard: Tree, nextBinder: Symbol, initialSubstitution: Substitution) extends FunTreeMaker {
      def chainBefore(next: Tree): Tree =
        pmgen.flatMap(extractor, wrapFunSubst(pmgen.condOptimized(guard, next))) setPos extractor.pos
    }

    // need to substitute since binder may be used outside of the next extractor call (say, in the body of the case)
    case class TypeTestTreeMaker(prevBinder: Symbol, nextBinderTp: Type, pos: Position) extends SimpleTreeMaker {
      val extractor = pmgen.condCast(typeTest(prevBinder, nextBinderTp), prevBinder, nextBinderTp)
    }

    // implements the run-time aspects of (§8.2) (typedPattern has already done the necessary type transformations)
    case class TypeAndEqualityTestTreeMaker(prevBinder: Symbol, patBinder: Symbol, pt: Type, pos: Position) extends SimpleTreeMaker {
      val nextBinderTp = glb(List(patBinder.info.widen, pt))
      val extractor = pmgen.condCast(typeAndEqualityTest(patBinder, pt), patBinder, nextBinderTp)
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, pos: Position) extends SimpleTreeMaker {
      val nextBinderTp: Type = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val extractor = atPos(pos)(pmgen.cond(pmgen._equals(patTree, prevBinder), CODE.REF(prevBinder), nextBinderTp))
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, alts: Tree*) extends SingleBinderTreeMaker with FreshFunTreeMaker {
      val nextBinderTp: Type = prevBinder.info.widen
      val pos = alts.head.pos
      def chainBefore(next: Tree): Tree =
        pmgen.or(wrapFunSubst(next), alts.toList) setPos alts.head.pos
    }

    case class GuardTreeMaker(guardTree: Tree) extends SingleExtractorTreeMaker {
      val initialSubstitution: Substitution = EmptySubstitution
      val nextBinder = freshSym(guardTree.pos, UnitClass.tpe)
      val extractor = pmgen.guard(guardTree)
    }

    // combineExtractors changes the current substitution's of the tree makers in `treeMakers`
    def combineExtractors(treeMakers: List[TreeMaker], body: Tree): Tree = {
      // a foldLeft to accumulate the initialSubstitution left-to-right, but written using a map and a var for clarity
      def propagateSubstitution(treeMakers: List[TreeMaker]): List[TreeMaker] = {
        var accumSubst: Substitution = EmptySubstitution
        treeMakers foreach { maker =>
          // could mutate maker instead, but it doesn't seem to shave much time off of quick.comp
          maker addOuterSubstitution accumSubst
          accumSubst = maker.substitution
        }
        treeMakers
      }

      propagateSubstitution(treeMakers).foldRight (body) (_ chainBefore _)
    //   this optimization doesn't give us much
    //   var accumSubst: Substitution = EmptySubstitution
    //   var revMakers: List[TreeMaker] = Nil
    //   treeMakers foreach { maker =>
    //     accumSubst = accumSubst >> maker.substitution
    //     maker.substitution = accumSubst
    //     revMakers ::= maker
    //   }
    //
    //   var accumTree = body
    //   revMakers foreach { maker =>
    //     accumTree = maker chainBefore accumTree
    //   }
    //
    //   atPos(pos)(accumTree)
    }

    def combineCases(scrut: Tree, scrutSym: Symbol, cases: List[(List[TreeMaker], Tree)], pt: Type): Tree = {
      val matcher =
        if (cases nonEmpty) {
          // when specified, need to propagate pt explicitly (type inferencer can't handle it)
          val optPt =
            if (isFullyDefined(pt)) appliedType(matchingMonadType, List(pt))
            else NoType

          // map + foldLeft
          var combinedCases = combineExtractors(cases.head._1, cases.head._2)
          cases.tail foreach { case (pats, body) =>
            combinedCases = pmgen.typedOrElse(optPt)(combinedCases, combineExtractors(pats, body))
          }

          pmgen.fun(scrutSym, combinedCases)
        } else pmgen.zero

      pmgen.runOrElse(scrut, matcher, scrutSym.info, if (isFullyDefined(pt)) pt else NoType)
    }

    object Substitution {
      def apply(from: Symbol, to: Tree) = new Substitution(List(from), List(to))
      // requires sameLength(from, to)
      def apply(from: List[Symbol], to: List[Tree]) =
        if (from nonEmpty) new Substitution(from, to) else EmptySubstitution
    }

    class Substitution(val from: List[Symbol], val to: List[Tree]) {
      def apply(tree: Tree): Tree = typedSubst(tree, from, to)
      // forall t: Tree. this(other(t)) == (this >> other)(t)
      def >>(other: Substitution): Substitution = {
        val (fromFiltered, toFiltered) = (from, to).zipped filter { (f, t) =>  !other.from.contains(f) }
        new Substitution(other.from ++ fromFiltered, other.to.map(apply) ++ toFiltered) // a quick benchmarking run indicates the `.map(apply)` is not too costly
      }
    }

    object EmptySubstitution extends Substitution(Nil, Nil) {
      override def apply(tree: Tree): Tree = tree
      override def >>(other: Substitution): Substitution = other
    }

    def matchingMonadType: Type
    def typedSubst(tree: Tree, from: List[Symbol], to: List[Tree]): Tree
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x"): Symbol
    def typeAndEqualityTest(patBinder: Symbol, pt: Type): Tree
    def typeTest(binder: Symbol, pt: Type): Tree

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodeGen { import CODE.UNIT
      def runOrElse(scrut: Tree, matcher: Tree, scrutTp: Type, resTp: Type): Tree
      def flatMap(a: Tree, b: Tree): Tree
      def fun(arg: Symbol, body: Tree): Tree
      def or(f: Tree, as: List[Tree]): Tree
      def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree
      def guard(c: Tree): Tree
      def zero: Tree
      // TODO: defaults in traits + self types == broken?
      // def guard(c: Tree, then: Tree, tp: Type): Tree
      // def cond(c: Tree): Tree = cond(c, UNIT, NoType)
      def cond(c: Tree, then: Tree, tp: Type): Tree
      def condOptimized(c: Tree, then: Tree): Tree
      def condCast(c: Tree, binder: Symbol, expectedTp: Type): Tree
      def _equals(checker: Tree, binder: Symbol): Tree
    }

    def pmgen: AbsCodeGen
  }

  // generate actual trees
  trait MatchCodeGen extends TreeMakers {
    def matchingStrategy: Tree
    def typed(tree: Tree, mode: Int, pt: Type): Tree // implemented in MatchTranslator

    @inline private def typedIfOrigTyped(to: Tree, origTp: Type): Tree =
      if (origTp == null || origTp == NoType) to
      // important: only type when actually substing and when original tree was typed
      // (don't need to use origTp as the expected type, though, and can't always do this anyway due to unknown type params stemming from polymorphic extractors)
      else typed(to, EXPRmode, WildcardType)

    // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
    // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
    def typedSubst(tree: Tree, from: List[Symbol], to: List[Tree]): Tree = {
      // according to -Ystatistics 10% of translateMatch's time is spent in this method...
      // since about half of the typedSubst's end up being no-ops, the check below shaves off 5% of the time spent in typedSubst
      if (!tree.exists { case i@Ident(_) => from contains i.symbol case _ => false}) tree
      else (new Transformer {
        override def transform(tree: Tree): Tree = {
          def subst(from: List[Symbol], to: List[Tree]): Tree =
            if (from.isEmpty) tree
            else if (tree.symbol == from.head) typedIfOrigTyped(to.head.shallowDuplicate, tree.tpe)
            else subst(from.tail, to.tail)

          tree match {
            case Ident(_) => subst(from, to)
            case _        => super.transform(tree)
          }
        }
      }).transform(tree)
    }

    lazy val pmgen: CommonCodeGen with MatchingStrategyGen with MonadInstGen =
      if (matchingMonadType.typeSymbol eq OptionClass) (new CommonCodeGen with MatchingStrategyGenOpt with MonadInstGenOpt {})
      else (new CommonCodeGen with MatchingStrategyGen with MonadInstGen {})

    var ctr = 0
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x") = {ctr += 1;
      // assert(owner ne null)
      // assert(owner ne NoSymbol)
      new TermSymbol(NoSymbol, pos, vpmName.counted(prefix, ctr)) setInfo repackExistential(tp)
    }

    def repeatedToSeq(tp: Type): Type = (tp baseType RepeatedParamClass) match {
      case TypeRef(_, RepeatedParamClass, args) => appliedType(SeqClass.typeConstructor, args)
      case _ => tp
    }

    object vpmName {
      val caseResult = "caseResult".toTermName
      val drop       = "drop".toTermName
      val flatMap    = "flatMap".toTermName
      val get        = "get".toTermName
      val guard      = "guard".toTermName
      val isEmpty    = "isEmpty".toTermName
      val one        = "one".toTermName
      val or         = "or".toTermName
      val orElse     = "orElse".toTermName
      val outer      = "<outer>".toTermName
      val runOrElse  = "runOrElse".toTermName
      val zero       = "zero".toTermName

      def counted(str: String, i: Int) = (str+i).toTermName
      def tupleIndex(i: Int) = ("_"+i).toTermName
    }

    import CODE._

    def typesConform(tp: Type, pt: Type) = ((tp eq pt) || (tp <:< pt))

    trait CommonCodeGen extends AbsCodeGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      def fun(arg: Symbol, body: Tree): Tree          = Function(List(ValDef(arg)), body)
      def tupleSel(binder: Symbol)(i: Int): Tree      = (REF(binder) DOT vpmName.tupleIndex(i)) // make tree that accesses the i'th component of the tuple referenced by binder
      def index(tgt: Tree)(i: Int): Tree              = tgt APPLY (LIT(i))
      def drop(tgt: Tree)(n: Int): Tree               = (tgt DOT vpmName.drop) (LIT(n))
      def _equals(checker: Tree, binder: Symbol): Tree = checker MEMBER_== REF(binder)          // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def and(a: Tree, b: Tree): Tree                 = a AND b

      // the force is needed mainly to deal with the GADT typing hack (we can't detect it otherwise as tp nor pt need contain an abstract type, we're just casting wildly)
      def _asInstanceOf(t: Tree, tp: Type, force: Boolean = false): Tree      = { val tpX = repackExistential(tp)
        if (!force && (t.tpe ne NoType) && t.isTyped && typesConform(t.tpe, tpX))  t //{ println("warning: emitted redundant asInstanceOf: "+(t, t.tpe, tp)); t } //.setType(tpX)
        else gen.mkAsInstanceOf(t, tpX, true, false)
      }

      def _isInstanceOf(b: Symbol, tp: Type): Tree    = gen.mkIsInstanceOf(REF(b), repackExistential(tp), true, false)
      // { val tpX = repackExistential(tp)
      //   if (typesConform(b.info, tpX)) { println("warning: emitted spurious isInstanceOf: "+(b, tp)); TRUE }
      //   else gen.mkIsInstanceOf(REF(b), tpX, true, false)
      // }

      def _asInstanceOf(b: Symbol, tp: Type): Tree    = { val tpX = repackExistential(tp)
        if (typesConform(b.info, tpX)) REF(b) //{ println("warning: emitted redundant asInstanceOf: "+(b, b.info, tp)); REF(b) } //.setType(tpX)
        else gen.mkAsInstanceOf(REF(b), tpX, true, false)
      }
    }

    trait MatchingStrategyGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      // methods in MatchingStrategy (the monad companion) -- used directly in translation
      def runOrElse(scrut: Tree, matcher: Tree, scrutTp: Type, resTp: Type): Tree = genTypeApply(matchingStrategy DOT vpmName.runOrElse, scrutTp, resTp) APPLY (scrut) APPLY (matcher)  // matchingStrategy.runOrElse(scrut)(matcher)
      def zero: Tree                                                     = matchingStrategy DOT vpmName.zero                                // matchingStrategy.zero
      def one(res: Tree, tp: Type = NoType, oneName: Name = vpmName.one): Tree = genTypeApply(matchingStrategy DOT oneName, tp) APPLY (res) // matchingStrategy.one(res)
      def or(f: Tree, as: List[Tree]): Tree                              = (matchingStrategy DOT vpmName.or)((f :: as): _*)                 // matchingStrategy.or(f, as)
      def guard(c: Tree): Tree                                           = (matchingStrategy DOT vpmName.guard)(c, UNIT) // matchingStrategy.guard(c, then) -- a user-defined guard
      // TODO: get rid of the cast when it's unnecessary, but this requires type checking `body` -- maybe this should be one of the optimisations we perform after generating the tree
      def caseResult(res: Tree, tp: Type): Tree                          = (matchingStrategy DOT vpmName.caseResult) (_asInstanceOf(res, tp, force = true)) // matchingStrategy.caseResult(res), like one, but blow this one away for isDefinedAt (since it's the RHS of a case)

      // an internal guard TODO: use different method call so exhaustiveness can distinguish it from user-defined guards
      def cond(c: Tree, then: Tree = UNIT, tp: Type = NoType): Tree = genTypeApply((matchingStrategy DOT vpmName.guard), repackExistential(tp)) APPLY (c, then) // matchingStrategy.guard(c, then)
      def condCast(c: Tree, binder: Symbol, expectedTp: Type): Tree = cond(c, _asInstanceOf(binder, expectedTp), expectedTp)
      def condOptimized(c: Tree, then: Tree): Tree                  = IF (c) THEN then ELSE zero
    }

    trait MonadInstGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      // methods in the monad instance -- used directly in translation
      def flatMap(a: Tree, b: Tree): Tree                                = (a DOT vpmName.flatMap)(b)
      def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree    = (genTypeApply(thisCase DOT vpmName.orElse, pt)) APPLY (elseCase)
    }

    // when we know we're targetting Option, do some inlining the optimizer won't do
    // `o.flatMap(f)` becomes `if(o == None) None else f(o.get)`, similarly for orElse and guard
    // this is a special instance of the advanced inlining optimization that takes a method call on
    // an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    trait MatchingStrategyGenOpt extends MatchingStrategyGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      override def guard(c: Tree): Tree = condOptimized(c, one(UNIT))
      override def cond(c: Tree, then: Tree = UNIT, tp: Type = NoType): Tree = condOptimized(c, one(then, repackExistential(tp)))
      // override def runOrElse(scrut: Tree, matcher: Tree): Tree = matcher match {
      //   case Function(List(x: ValDef), body) =>
      //     val tp      = x.symbol.tpe
      //     val restp   = appliedType(matchingMonadType, List(pt)) // don't always know pt....
      //     val isEmpty = restp member vpmName.isEmpty
      //     val get     = restp member vpmName.get
      //
      //     val vs      = freshSym(scrut.pos, tp, "s")
      //     val vres    = freshSym(scrut.pos, restp, "res")
      //     val s       = VAL(vs) === scrut
      //     val res     = VAL(vres) === typedSubst(body, List(x.symbol), List(REF(vs)))
      //
      //     BLOCK(
      //       s,
      //       res,
      //       IF (res DOT isEmpty) THEN  ELSE (res DOT get)
      //     )
      // }
    }

    trait MonadInstGenOpt extends MonadInstGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      override def flatMap(opt: Tree, fun: Tree): Tree = fun match {
        case Function(List(x: ValDef), body) =>
          val tp      = appliedType(matchingMonadType, List(x.symbol.tpe))
          val vs      = freshSym(opt.pos, tp, "o")
          val isEmpty = tp member vpmName.isEmpty
          val get     = tp member vpmName.get
          val v       = VAL(vs) === opt

          BLOCK(
            v,
            IF (vs DOT isEmpty) THEN zero ELSE typedSubst(body, List(x.symbol), List(vs DOT get))
          )
        case _ => println("huh?")
          (opt DOT vpmName.flatMap)(fun)
      }
      override def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree = {
        val vs = freshSym(thisCase.pos, pt, "o")
        val isEmpty = pt member vpmName.isEmpty
        val v = VAL(vs) === thisCase // genTyped(, pt)
        BLOCK(
          v,
          IF (vs DOT isEmpty) THEN elseCase /*genTyped(, pt)*/ ELSE REF(vs)
        )
      }
    }

    def genTypeApply(tfun: Tree, args: Type*): Tree                       = if(args contains NoType) tfun else TypeApply(tfun, args.toList map TypeTree)
    // def genTyped(t: Tree, tp: Type): Tree                                 = if(tp == NoType) t else Typed(t, TypeTree(repackExistential(tp)))
  }


  // TODO: do this during tree construction, but that will require tracking the current owner in treemakers
  // TODO: assign more fine-grained positions
  // fixes symbol nesting, assigns positions
  def fixerUpper(origOwner: Symbol, pos: Position) = new Traverser {
    currentOwner = origOwner

    override def traverse(t: Tree) {
      if (t != EmptyTree && t.pos == NoPosition) {
        t.setPos(pos)
      }
      t match {
        case Function(_, _) if t.symbol == NoSymbol =>
          t.symbol = currentOwner.newValue(t.pos, nme.ANON_FUN_NAME).setFlag(SYNTHETIC).setInfo(NoType)
          // println("new symbol for "+ (t, t.symbol.ownerChain))
        case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == origOwner) =>
          // println("fundef: "+ (t, t.symbol.ownerChain, currentOwner.ownerChain))
          t.symbol.owner = currentOwner
        case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == origOwner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
          // println("def: "+ (d, d.symbol.ownerChain, currentOwner.ownerChain))
          if(d.symbol.isLazy) { // for lazy val's accessor -- is there no tree??
            assert(d.symbol.lazyAccessor != NoSymbol && d.symbol.lazyAccessor.owner == d.symbol.owner)
            d.symbol.lazyAccessor.owner = currentOwner
          }
          if(d.symbol.moduleClass ne NoSymbol)
            d.symbol.moduleClass.owner = currentOwner

          d.symbol.owner = currentOwner
        // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
        //   println("untouched "+ (t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain))
        case _ =>
      }
      super.traverse(t)
    }

    // override def apply
    // println("before fixerupper: "+ xTree)
    // currentRun.trackerFactory.snapshot()
    // println("after fixerupper")
    // currentRun.trackerFactory.snapshot()
  }
}

// object noShadowedUntyped extends Traverser {
//   override def traverse(t: Tree) {
//     if ((t.tpe ne null) && (t.tpe ne NoType)) okTree = t
//     else if(okTree ne null) println("untyped subtree "+ t +" in typed tree"+ okTree +" : "+ okTree.tpe)
//     super.traverse(t)
//   }
//   var okTree: Tree = null
// }
// private def c(t: Tree): Tree = noShadowedUntyped(t)
