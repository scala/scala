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
      val okPt = repeatedToSeq(pt)
      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      combineCases(scrut, scrutSym, cases map translateCase(scrutSym, okPt), okPt, context.owner)
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
    def translateCase(scrutSym: Symbol, pt: Type)(caseDef: CaseDef) = caseDef match { case CaseDef(pattern, guard, body) =>
      translatePattern(scrutSym, pattern) ++ translateGuard(guard) :+ translateBody(body, pt)
    }

    def translatePattern(patBinder: Symbol, patTree: Tree): List[TreeMaker] = {
      // a list of TreeMakers that encode `patTree`, and a list of arguments for recursive invocations of `translatePattern` to encode its subpatterns
      type TranslationStep = (List[TreeMaker], List[(Symbol, Tree)])
      @inline def withSubPats(treeMakers: List[TreeMaker], subpats: (Symbol, Tree)*): TranslationStep = (treeMakers, subpats.toList)
      @inline def noFurtherSubPats(treeMakers: TreeMaker*): TranslationStep = (treeMakers.toList, Nil)

      val pos = patTree.pos

      def translateExtractorPattern(extractor: ExtractorCall): TranslationStep = {
        if (!extractor.isTyped) throw new TypeError(pos, "Could not typecheck extractor call: "+ extractor)
        // if (extractor.resultInMonad == ErrorType) throw new TypeError(pos, "Unsupported extractor type: "+ extractor.tpe)

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
          noFurtherSubPats(AlternativesTreeMaker(patBinder, alts map (translatePattern(patBinder, _)), alts.head.pos))

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

    // TODO: 1) if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (which is now done by pmgen.one),
    // so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand?
    // to enable this, probably need to move away from Option to a monad specific to pattern-match,
    // so that we can return Option's from a match without ambiguity whether this indicates failure in the monad, or just some result in the monad
    // 2) body.tpe is the type of the body after applying the substitution that represents the solution of GADT type inference
    // need the explicit cast in case our substitutions in the body change the type to something that doesn't take GADT typing into account
    def translateBody(body: Tree, matchPt: Type): TreeMaker =
      BodyTreeMaker(body, matchPt)


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// helper methods: they analyze types and trees in isolation, but they are not (directly) concerned with the structure of the overall translation
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    object ExtractorCall {
      def apply(unfun: Tree, args: List[Tree]): ExtractorCall = new ExtractorCallRegular(unfun, args)

      def fromCaseClass(fun: Tree, args: List[Tree]): Option[ExtractorCall] =  Some(new ExtractorCallProd(fun, args))

      // THE PRINCIPLED SLOW PATH -- NOT USED
      // generate a call to the (synthetically generated) extractor of a case class
      // NOTE: it's an apply, not a select, since in general an extractor call may have multiple argument lists (including an implicit one)
      // that we need to preserve, so we supply the scrutinee as Ident(nme.SELECTOR_DUMMY),
      // and replace that dummy by a reference to the actual binder in translateExtractorPattern
      def fromCaseClassUnapply(fun: Tree, args: List[Tree]): Option[ExtractorCall] = {
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

    abstract class ExtractorCall(val args: List[Tree]) {
      val nbSubPats = args.length

      // everything okay, captain?
      def isTyped    : Boolean

      def isSeq: Boolean
      lazy val lastIsStar = (nbSubPats > 0) && treeInfo.isStar(args.last)

      // to which type should the previous binder be casted?
      def paramType  : Type

      // binder has been casted to paramType if necessary
      def treeMaker(binder: Symbol, pos: Position): TreeMaker

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

      def subPatTypes: List[Type] =
        if(isSeq) {
          val TypeRef(pre, SeqClass, args) = seqTp
          // do repeated-parameter expansion to match up with the expected number of arguments (in casu, subpatterns)
          formalTypes(rawSubPatTypes.init :+ typeRef(pre, RepeatedParamClass, args), nbSubPats)
        } else rawSubPatTypes

      protected def rawSubPatTypes: List[Type]

      protected def seqTp = rawSubPatTypes.last baseType SeqClass
      protected def seqLenCmp                = rawSubPatTypes.last member nme.lengthCompare
      protected lazy val firstIndexingBinder = rawSubPatTypes.length - 1 // rawSubPatTypes.last is the Seq, thus there are `rawSubPatTypes.length - 1` non-seq elements in the tuple
      protected lazy val lastIndexingBinder  = if(lastIsStar) nbSubPats-2 else nbSubPats-1
      protected lazy val expectedLength      = lastIndexingBinder - firstIndexingBinder + 1
      protected lazy val minLenToCheck       = if(lastIsStar) 1 else 0
      protected def seqTree(binder: Symbol)  = tupleSel(binder)(firstIndexingBinder+1)
      protected def tupleSel(binder: Symbol)(i: Int): Tree = pmgen.tupleSel(binder)(i)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require isSeq
      protected def subPatRefsSeq(binder: Symbol): List[Tree] = {
        // only relevant if isSeq: (here to avoid capturing too much in the returned closure)
        val indexingIndices               = (0 to (lastIndexingBinder-firstIndexingBinder))
        val nbIndexingIndices             = indexingIndices.length

        // this error is checked by checkStarPatOK
        // if(isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if(lastIsStar) 1 else 0) == nbSubPats, "(resultInMonad, ts, subPatTypes, subPats)= "+(resultInMonad, ts, subPatTypes, subPats))
        // there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
        (((1 to firstIndexingBinder) map tupleSel(binder)) ++
        // then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
        (indexingIndices map pmgen.index(seqTree(binder))) ++
        // the last one -- if the last subpattern is a sequence wildcard: drop the prefix (indexed by the refs on the line above), return the remainder
        (if(!lastIsStar) Nil else List(
          if(nbIndexingIndices == 0) seqTree(binder)
          else pmgen.drop(seqTree(binder))(nbIndexingIndices)))).toList
      }

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      protected def subPatRefs(binder: Symbol): List[Tree] = {
        if (nbSubPats == 0) Nil
        else if (isSeq) subPatRefsSeq(binder)
        else ((1 to nbSubPats) map tupleSel(binder)).toList
      }

      protected def lengthGuard(binder: Symbol): Option[Tree] =
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
    }

    // TODO: to be called when there's a def unapplyProd(x: T): Product_N
    // for now only used for case classes -- pretending there's an unapplyProd that's the identity (and don't call it)
    class ExtractorCallProd(fun: Tree, args: List[Tree]) extends ExtractorCall(args) {
      // TODO: fix the illegal type bound in pos/t602 -- type inference messes up before we get here:
      /*override def equals(x$1: Any): Boolean = ...
             val o5: Option[com.mosol.sl.Span[Any]] =  // Span[Any] --> Any is not a legal type argument for Span!
      */
      // private val orig            = fun match {case tpt: TypeTree => tpt.original case _ => fun}
      // private val origExtractorTp = unapplyMember(orig.symbol.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe).tpe
      // private val extractorTp     = if (wellKinded(fun.tpe)) fun.tpe else existentialAbstraction(origExtractorTp.typeParams, origExtractorTp.resultType)
      // println("ExtractorCallProd: "+ (fun.tpe, existentialAbstraction(origExtractorTp.typeParams, origExtractorTp.resultType)))
      // println("ExtractorCallProd: "+ (fun.tpe, args map (_.tpe)))
      private def extractorTp = fun.tpe

      def isTyped    = fun.isTyped

      // to which type should the previous binder be casted?
      def paramType  = extractorTp.finalResultType

      def isSeq: Boolean = rawSubPatTypes.nonEmpty && isRepeatedParamType(rawSubPatTypes.last)
      protected def rawSubPatTypes = extractorTp.paramTypes

      // binder has type paramType
      def treeMaker(binder: Symbol, pos: Position): TreeMaker = {
        // checks binder ne null before chaining to the next extractor
        ProductExtractorTreeMaker(binder, lengthGuard(binder), Substitution(subPatBinders, subPatRefs(binder)))
      }

/* TODO: remove special case when the following bug is fixed
scala> :paste
// Entering paste mode (ctrl-D to finish)

class Foo(x: Other) { x._1 } // BUG: can't refer to _1 if its defining class has not been type checked yet
case class Other(y: String)

// Exiting paste mode, now interpreting.

<console>:8: error: value _1 is not a member of Other
       class Foo(x: Other) { x._1 }
                               ^

scala> case class Other(y: String)
defined class Other

scala> class Foo(x: Other) { x._1 }
defined class Foo */
      override protected def tupleSel(binder: Symbol)(i: Int): Tree = { import CODE._
        // reference the (i-1)th case accessor if it exists, otherwise the (i-1)th tuple component
        val caseAccs = binder.info.typeSymbol.caseFieldAccessors
        if (caseAccs isDefinedAt (i-1)) REF(binder) DOT caseAccs(i-1)
        else pmgen.tupleSel(binder)(i)
      }

      override def toString(): String = "case class "+ (if (extractorTp eq null) fun else paramType.typeSymbol) +" with arguments "+ args
    }

    class ExtractorCallRegular(extractorCallIncludingDummy: Tree, args: List[Tree]) extends ExtractorCall(args) {
      private lazy val Some(Apply(extractorCall, _)) = extractorCallIncludingDummy.find{ case Apply(_, List(Ident(nme.SELECTOR_DUMMY))) => true case _ => false }

      def tpe        = extractorCall.tpe
      def isTyped    = (tpe ne NoType) && extractorCall.isTyped && (resultInMonad ne ErrorType)
      def paramType  = tpe.paramTypes.head
      def resultType = tpe.finalResultType
      def isSeq      = extractorCall.symbol.name == nme.unapplySeq

      def treeMaker(patBinderOrCasted: Symbol, pos: Position): TreeMaker = {
        // the extractor call (applied to the binder bound by the flatMap corresponding to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = atPos(pos)(spliceApply(patBinderOrCasted))
        val binder         = freshSym(pos, resultInMonad) // can't simplify this when subPatBinders.isEmpty, since UnitClass.tpe is definitely wrong when isSeq, and resultInMonad should always be correct since it comes directly from the extractor's result type
        ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder, Substitution(subPatBinders, subPatRefs(binder)))(resultType.typeSymbol == BooleanClass)
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
            case Apply(x, List(Ident(nme.SELECTOR_DUMMY))) =>
              treeCopy.Apply(t, x, List(CODE.REF(binder)))
            case _ => super.transform(t)
          }
        }
        splice.transform(extractorCallIncludingDummy)
      }

      // what's the extractor's result type in the monad?
      // turn an extractor's result type into something `monadTypeToSubPatTypesAndRefs` understands
      protected lazy val resultInMonad: Type = if(!hasLength(tpe.paramTypes, 1)) ErrorType else {
        if (resultType.typeSymbol == BooleanClass) UnitClass.tpe
        else {
          val monadArgs = resultType.baseType(matchingMonadType.typeSymbol).typeArgs
          // assert(monadArgs.length == 1, "unhandled extractor type: "+ extractorTp) // TODO: overloaded unapply??
          if(monadArgs.length == 1) monadArgs(0)
          else ErrorType
        }
      }

      protected lazy val rawSubPatTypes =
        if (resultInMonad.typeSymbol eq UnitClass) Nil
        else if(nbSubPats == 1)                    List(resultInMonad)
        else getProductArgs(resultInMonad) match {
          case Nil => List(resultInMonad)
          case x   => x
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
// the making of the trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait TreeMakers {
    def inMatchMonad(tp: Type): Type = appliedType(matchingMonadType, List(tp))
    lazy val optimizingCodeGen       = matchingMonadType.typeSymbol eq OptionClass

    abstract class TreeMaker {
      def substitution: Substitution =
        if (currSub eq null) localSubstitution
        else currSub

      protected def localSubstitution: Substitution

      private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        if (currSub ne null) {
          println("BUG: incorporateOuterSubstitution called more than once for "+ (this, currSub, outerSubst))
          Thread.dumpStack()
        }
        else currSub = outerSubst >> substitution
      }
      private[this] var currSub: Substitution = null

      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree, pt: Type): Tree
      def treesToHoist: List[Tree] = Nil
    }

    case class TrivialTreeMaker(tree: Tree) extends TreeMaker {
      val localSubstitution: Substitution = EmptySubstitution
      def chainBefore(next: Tree, pt: Type): Tree = tree
    }

    case class BodyTreeMaker(body: Tree, matchPt: Type) extends TreeMaker {
      val localSubstitution: Substitution = EmptySubstitution
      def chainBefore(next: Tree, pt: Type): Tree = // assert(next eq EmptyTree)
        atPos(body.pos)(substitution(pmgen.one(body, body.tpe, matchPt))) // since SubstOnly treemakers are dropped, need to do it here
    }

    case class SubstOnlyTreeMaker(localSubstitution: Substitution) extends TreeMaker {
      def chainBefore(next: Tree, pt: Type): Tree = substitution(next)
    }

    abstract class FunTreeMaker extends TreeMaker {
      val nextBinder: Symbol

      // for CSE (used iff optimizingCodeGen)
      // TODO: factor this out -- don't mutate treemakers
      var reused: Boolean = false
      def reusedBinders: List[Symbol] = Nil
      override def treesToHoist: List[Tree] = { import CODE._
        reusedBinders map { b => VAL(b) === pmgen.mkZero(b.info) }
      }
    }

    abstract class FreshFunTreeMaker extends FunTreeMaker {
      val pos: Position
      val prevBinder: Symbol
      val nextBinderTp: Type
      lazy val nextBinder = freshSym(pos, nextBinderTp)
      lazy val localSubstitution = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))
    }

    // TODO: factor out optimization-specific stuff into codegen
    abstract class CondTreeMaker extends FreshFunTreeMaker { import CODE._
      val cond: Tree
      val res: Tree

      // for CSE (used iff optimizingCodeGen)
      // must set reused before!
      override lazy val reusedBinders = if(reused) List(freshSym(pos, BooleanClass.tpe, "rc") setFlag MUTABLE, nextBinder setFlag MUTABLE) else Nil
      def storedCond         = reusedBinders(0)
      def storedRes          = reusedBinders(1)

      def chainBefore(next: Tree, pt: Type): Tree =
        if (!reused)
          atPos(pos)(pmgen.flatMapCond(cond, res, nextBinder, nextBinderTp, substitution(next)))
        else { // for CSE (used iff optimizingCodeGen)
          IF (cond) THEN BLOCK(
            storedCond === TRUE,
            storedRes  === res,
            substitution(next).duplicate // TODO: finer-grained dup'ing
          ) ELSE pmgen.zero
        }
    }

    // for CSE (used iff optimizingCodeGen)
    case class ReusingCondTreeMaker(dropped_priors: List[(TreeMaker, Option[TreeMaker])]) extends TreeMaker { import CODE._
      lazy val localSubstitution = {
        val (from, to) = dropped_priors.collect {case (dropped: CondTreeMaker, Some(prior: CondTreeMaker)) => (dropped.nextBinder, REF(prior.storedRes))}.unzip
        val oldSubs = dropped_priors.collect {case (dropped: TreeMaker, _) => dropped.substitution}
        oldSubs.foldLeft(Substitution(from, to))(_ >> _)
      }

      def chainBefore(next: Tree, pt: Type): Tree = {
        val cond = REF(dropped_priors.reverse.collectFirst{case (_, Some(ctm: CondTreeMaker)) => ctm}.get.storedCond)

        IF (cond) THEN BLOCK(
          substitution(next).duplicate // TODO: finer-grained duplication -- MUST duplicate though, or we'll get VerifyErrors since sharing trees confuses lambdalift, and its confusion it emits illegal casts (diagnosed by Grzegorz: checkcast T ; invokevirtual S.m, where T not a subtype of S)
        ) ELSE pmgen.zero
      }
    }

    /**
     * Make a TreeMaker that will result in an extractor call specified by `extractor`
     * the next TreeMaker (here, we don't know which it'll be) is chained after this one by flatMap'ing
     * a function with binder `nextBinder` over our extractor's result
     * the function's body is determined by the next TreeMaker
     * in this function's body, and all the subsequent ones, references to the symbols in `from` will be replaced by the corresponding tree in `to`
     */
    case class ExtractorTreeMaker(extractor: Tree, extraCond: Option[Tree], nextBinder: Symbol, localSubstitution: Substitution)(extractorReturnsBoolean: Boolean) extends FunTreeMaker {
      def chainBefore(next: Tree, pt: Type): Tree = atPos(extractor.pos)(
        if (extractorReturnsBoolean) pmgen.flatMapCond(extractor, CODE.UNIT, nextBinder, nextBinder.info.widen, substitution(condAndNext(next)))
        else pmgen.flatMap(extractor, pmgen.fun(nextBinder, substitution(condAndNext(next))))
      )

      private def condAndNext(next: Tree): Tree = extraCond map (pmgen.condOptimized(_, next)) getOrElse next

      override def toString = "X"+(extractor, nextBinder)
    }

    // TODO: allow user-defined unapplyProduct
    case class ProductExtractorTreeMaker(prevBinder: Symbol, extraCond: Option[Tree], localSubstitution: Substitution) extends TreeMaker { import CODE._
      def chainBefore(next: Tree, pt: Type): Tree = {
        val nullCheck = REF(prevBinder) OBJ_NE NULL
        val cond = extraCond match {
          case None => nullCheck
          case Some(c) => nullCheck AND c
        }
        pmgen.condOptimized(cond, substitution(next))
      }

      override def toString = "P"+(prevBinder,  extraCond getOrElse "", localSubstitution)
    }


    // need to substitute since binder may be used outside of the next extractor call (say, in the body of the case)
    case class TypeTestTreeMaker(prevBinder: Symbol, nextBinderTp: Type, pos: Position) extends CondTreeMaker {
      val cond = typeTest(prevBinder, nextBinderTp)
      val res  = pmgen._asInstanceOf(prevBinder, nextBinderTp)
      override def toString = "TT"+(prevBinder, nextBinderTp)
    }

    // implements the run-time aspects of (§8.2) (typedPattern has already done the necessary type transformations)
    case class TypeAndEqualityTestTreeMaker(prevBinder: Symbol, patBinder: Symbol, pt: Type, pos: Position) extends CondTreeMaker {
      val nextBinderTp = glb(List(patBinder.info.widen, pt))

      val cond = typeAndEqualityTest(patBinder, pt)
      val res  = pmgen._asInstanceOf(patBinder, nextBinderTp)
      override def toString = "TET"+(patBinder, pt)
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, pos: Position) extends CondTreeMaker {
      val nextBinderTp = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = pmgen._equals(patTree, prevBinder)
      val res  = CODE.REF(prevBinder)
      override def toString = "ET"+(prevBinder, patTree)
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, var altss: List[List[TreeMaker]], pos: Position) extends TreeMaker {
      // don't substitute prevBinder to nextBinder, a set of alternatives does not need to introduce a new binder, simply reuse the previous one
      val localSubstitution: Substitution = EmptySubstitution

      override private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        super.incorporateOuterSubstitution(outerSubst)
        altss = altss map (alts => propagateSubstitution(alts, substitution))
      }

      def chainBefore(next: Tree, pt: Type): Tree = { import CODE._
        // next does not contain deftrees, is pretty short
        val canDuplicate = {
          var okToInline = true
          var sizeBudget = 100 / (altss.length max 1)  // yep, totally arbitrary!
          object travOkToInline extends Traverser { override def traverse(tree: Tree): Unit = if (sizeBudget >= 0) { sizeBudget -= 1; tree match {
            case TypeApply(_, _) | Apply(_, _) | Select(_, _)
               | Block(_, _) | Assign(_, _) | If(_, _, _) | Typed(_, _) => super.traverse(tree) // these are allowed if their subtrees are
            case EmptyTree | This(_) | New(_) | Literal(_) | Ident(_)   => // these are always ok
            case _ if tree.isType                                       => // these are always ok
            case _                                                      => okToInline = false //; println("not inlining: "+ (tree, tree.getClass))
          }}}
          travOkToInline.traverse(next)
          // println("(okToInline, sizeBudget): "+ (okToInline, sizeBudget))
          okToInline && sizeBudget > 0 // must be strict comparison
        }

        atPos(pos)(
          if (canDuplicate) {
            altss map {altTreeMakers =>
              combineExtractors(altTreeMakers :+ TrivialTreeMaker(substitution(next).duplicate), pt)
            } reduceLeft pmgen.typedOrElse(pt)
          } else {
            val rest = freshSym(pos, functionType(List(), inMatchMonad(pt)), "rest")
            // rest.info.member(nme.apply).withAnnotation(AnnotationInfo(ScalaInlineClass.tpe, Nil, Nil))

            // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
            // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
            val combinedAlts = altss map (altTreeMakers =>
              combineExtractors(altTreeMakers :+ TrivialTreeMaker(REF(rest) APPLY ()), pt)
            )
            BLOCK(
              VAL(rest) === Function(Nil, substitution(next)),
              combinedAlts reduceLeft pmgen.typedOrElse(pt)
            )
          }
        )
      }
    }

    case class GuardTreeMaker(guardTree: Tree) extends TreeMaker {
      val localSubstitution: Substitution = EmptySubstitution
      def chainBefore(next: Tree, pt: Type): Tree = pmgen.flatMapGuard(substitution(guardTree), next)
      override def toString = "G("+ guardTree +")"
    }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// decisions, decisions
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    object Test {
      var currId = 0
    }
    case class Test(cond: Cond, treeMaker: TreeMaker) {
      // def <:<(other: Test) = cond <:< other.cond
      // def andThen_: (prev: List[Test]): List[Test] =
      //   prev.filterNot(this <:< _) :+ this

      private val reusedBy = new collection.mutable.HashSet[Test]
      var reuses: Option[Test] = None
      def registerReuseBy(later: Test): Unit = {
        assert(later.reuses.isEmpty)
        reusedBy += later
        later.reuses = Some(this)
      }

      val id = { Test.currId += 1; Test.currId}
      override def toString =
        if (cond eq Top) "T"
        else if(cond eq Havoc) "!?"
        else "T"+ id + (if(reusedBy nonEmpty) "!["+ treeMaker +"]" else (if(reuses.isEmpty) "["+ treeMaker +"]" else  " cf. T"+reuses.get.id))
    }

    object Cond {
      // def refines(self: Cond, other: Cond): Boolean = (self, other) match {
      //   case (Bottom, _)       => true
      //   case (Havoc , _)       => true
      //   case (_         , Top) => true
      //   case (_         , _)   => false
      // }
      var currId = 0
    }

    abstract class Cond {
      // def testedPath: Tree
      // def <:<(other: Cond) = Cond.refines(this, other)

      val id = { Cond.currId += 1; Cond.currId}
    }

    // does not contribute any knowledge
    case object Top extends Cond

    // takes away knowledge. e.g., a user-defined guard
    case object Havoc extends Cond

    // we know everything! everything!
    // this either means the case is unreachable,
    // or that it is statically known to be picked -- at this point in the decision tree --> no point in emitting further alternatives
    // case object Bottom extends Cond


    object EqualityCond {
      private val uniques = new collection.mutable.HashMap[(Tree, Tree), EqualityCond]
      def apply(testedPath: Tree, rhs: Tree): EqualityCond = uniques getOrElseUpdate((testedPath, rhs), new EqualityCond(testedPath, rhs))
    }
    class EqualityCond(testedPath: Tree, rhs: Tree) extends Cond {
      // def negation         = TopCond // inequality doesn't teach us anything
      // do simplification when we know enough about the tree statically:
      //  - collapse equal trees
      //  - accumulate tests when (in)equality not known statically
      //  - become bottom when we statically know this can never match

      override def toString = testedPath +" == "+ rhs +"#"+ id
    }

    object TypeCond {
      private val uniques = new collection.mutable.HashMap[(Tree, Type), TypeCond]
      def apply(testedPath: Tree, pt: Type): TypeCond = uniques getOrElseUpdate((testedPath, pt), new TypeCond(testedPath, pt))
    }
    class TypeCond(testedPath: Tree, pt: Type) extends Cond {
      // def negation         = TopCond // inequality doesn't teach us anything
      // do simplification when we know enough about the tree statically:
      //  - collapse equal trees
      //  - accumulate tests when (in)equality not known statically
      //  - become bottom when we statically know this can never match
      override def toString = testedPath +" <: "+ pt +"#"+ id
    }

    object TypeAndEqualityCond {
      private val uniques = new collection.mutable.HashMap[(Tree, Type), TypeAndEqualityCond]
      def apply(testedPath: Tree, pt: Type): TypeAndEqualityCond = uniques getOrElseUpdate((testedPath, pt), new TypeAndEqualityCond(testedPath, pt))
    }
    class TypeAndEqualityCond(testedPath: Tree, pt: Type) extends Cond {
      // def negation         = TopCond // inequality doesn't teach us anything
      // do simplification when we know enough about the tree statically:
      //  - collapse equal trees
      //  - accumulate tests when (in)equality not known statically
      //  - become bottom when we statically know this can never match
      override def toString = testedPath +" (<: && ==) "+ pt +"#"+ id
    }

    /** a flow-sensitive, generalised, common sub-expression elimination
     * reuse knowledge from performed tests
     * the only sub-expressions we consider are the conditions and results of the three tests (type, type&equality, equality)
     * when a sub-expression is share, it is stored in a mutable variable
     * the variable is floated up so that its scope includes all of the program that shares it
     * we generalize sharing to implication, where b reuses a if a => b and priors(a) => priors(b) (the priors of a sub expression form the path through the decision tree)
     *
     * intended to be generalised to exhaustivity/reachability checking
     */
    def doCSE(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[List[TreeMaker]] = {
      // a variable in this set should never be replaced by a tree that "does not consist of a selection on a variable in this set" (intuitively)
      val pointsToBound            = collection.mutable.HashSet(prevBinder)

      // the substitution that renames variables to variables in pointsToBound
      var normalize: Substitution  = EmptySubstitution

      // replaces a variable (in pointsToBound) by a selection on another variable in pointsToBound
      // TODO check:
      //   pointsToBound -- accumSubst.from == Set(prevBinder) && (accumSubst.from.toSet -- pointsToBound) isEmpty
      var accumSubst: Substitution = EmptySubstitution

      val trees = new collection.mutable.HashSet[Tree]

      def approximateTreeMaker(tm: TreeMaker): Test = {
        val subst = tm.substitution

        // find part of substitution that replaces bound symbols by new symbols, and reverse that part
        // so that we don't introduce new aliases for existing symbols, thus keeping the set of bound symbols minimal
        val (boundSubst, unboundSubst) = (subst.from zip subst.to) partition {case (f, t) =>
            t.isInstanceOf[Ident] && (t.symbol ne NoSymbol) && pointsToBound(f)
          }
        val (boundFrom, boundTo) = boundSubst.unzip
        normalize >>= Substitution(boundTo map (_.symbol), boundFrom map (CODE.REF(_)))
        // println("normalize: "+ normalize)

        val (unboundFrom, unboundTo) = unboundSubst unzip
        val okSubst = Substitution(unboundFrom, unboundTo map (normalize(_))) // it's important substitution does not duplicate trees here -- it helps to keep hash consing simple, anyway
        pointsToBound ++= ((okSubst.from, okSubst.to).zipped filter { (f, t) => pointsToBound exists (sym => t.exists(_.symbol == sym)) })._1
        // println("pointsToBound: "+ pointsToBound)

        accumSubst >>= okSubst
        // println("accumSubst: "+ accumSubst)

        // TODO: improve, e.g., for constants
        def sameValue(a: Tree, b: Tree): Boolean = (a eq b) || ((a, b) match {
          case (_ : Ident, _ : Ident) => a.symbol eq b.symbol
          case _                      => false
        })

        // hashconsing trees (modulo value-equality)
        def unique(t: Tree): Tree =
          trees find (a => a.equalsStructure0(t)(sameValue)) match {
            case Some(orig) => orig // println("unique: "+ (t eq orig, orig));
            case _ => trees += t; t
          }

        def uniqueTp(tp: Type): Type = tp match {
          // typerefs etc are already hashconsed
          case _ : UniqueType                      => tp
          case tp@RefinedType(parents, EmptyScope) => tp.memo(tp: Type)(identity) // TODO: does this help?
          case _                                   => tp
        }

        def binderToUniqueTree(b: Symbol) = unique(accumSubst(normalize(CODE.REF(b))))

        Test(tm match {
          case ProductExtractorTreeMaker(pb, None, subst)           => Top // TODO: NotNullTest(prevBinder)
          case tm@TypeTestTreeMaker(prevBinder, nextBinderTp, _)    => TypeCond(binderToUniqueTree(prevBinder), uniqueTp(nextBinderTp))
          case tm@TypeAndEqualityTestTreeMaker(_, patBinder, pt, _) => TypeAndEqualityCond(binderToUniqueTree(patBinder), uniqueTp(pt))
          case tm@EqualityTestTreeMaker(prevBinder, patTree, _)     => EqualityCond(binderToUniqueTree(prevBinder), unique(patTree))
          case ExtractorTreeMaker(_, _, _, _)
             | GuardTreeMaker(_)
             | ProductExtractorTreeMaker(_, Some(_), _)             => Havoc
          case AlternativesTreeMaker(_, _, _)                       => Havoc // TODO: can do better here
          case SubstOnlyTreeMaker(_)                                => Top
          case BodyTreeMaker(_, _)                                  => Havoc
        }, tm)
      }

      val testss = cases.map { _ map approximateTreeMaker }

      // interpret:
      val dependencies = new collection.mutable.LinkedHashMap[Test, Set[Cond]]
      val tested = new collection.mutable.HashSet[Cond]
      testss foreach { tests =>
        tested.clear()
        tests dropWhile { test =>
          val cond = test.cond
          if ((cond eq Havoc) || (cond eq Top)) (cond eq Top) // stop when we encounter a havoc, skip top
          else {
            tested += cond

            // is there an earlier test that checks our condition and whose dependencies are implied by ours?
            dependencies find { case (priorTest, deps) =>
              ((priorTest.cond eq cond) || (deps contains cond)) && (deps subsetOf tested)
            } foreach { case (priorTest, deps) =>
              // if so, note the dependency in both tests
              priorTest registerReuseBy test
            }

            dependencies(test) = tested.toSet // copies
            true
          }
        }
      }

      // find longest prefix of tests that reuse a prior test, and whose dependent conditions monotonically increase
      // then, collapse these contiguous sequences of reusing tests
      // store the result of the final test and the intermediate results in hoisted mutable variables (TODO: optimize: don't store intermediate results that aren't used)
      // replace each reference to a variable originally bound by a collapsed test by a reference to the hoisted variable
      testss map { tests =>
        var currDeps = Set[Cond]()
        val (sharedPrefix, suffix) = tests span { test =>
          (test.cond eq Top) || (for(
              reusedTest <- test.reuses;
              nextDeps <- dependencies.get(reusedTest);
              diff <- (nextDeps -- currDeps).headOption;
              _ <- Some(currDeps = nextDeps))
                yield diff).nonEmpty
        }

        val collapsedTreeMakers = if (sharedPrefix.nonEmpty) { // even sharing prefixes of length 1 brings some benefit (overhead-percentage for compiler: 26->24%, lib: 19->16%)
          for (test <- sharedPrefix; reusedTest <- test.reuses; if reusedTest.treeMaker.isInstanceOf[FunTreeMaker])
            reusedTest.treeMaker.asInstanceOf[FunTreeMaker].reused = true
          // println("sharedPrefix: "+ sharedPrefix)
          for (lastShared <- sharedPrefix.reverse.dropWhile(_.cond eq Top).headOption;
               lastReused <- lastShared.reuses)
            yield ReusingCondTreeMaker(sharedPrefix map (t => (t.treeMaker, t.reuses map (_.treeMaker)))) :: suffix.map(_.treeMaker)
        } else None

        collapsedTreeMakers getOrElse tests.map(_.treeMaker) // sharedPrefix need not be empty (but it only contains Top-tests, which are dropped above)
      }
    }

    // TODO: non-trivial dead-code elimination
    // e.g., the following match should compile to a simple instanceof:
    //   case class Ident(name: String)
    //   for (Ident(name) <- ts) println(name)
    def doDCE(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[List[TreeMaker]] = {
      // do minimal DCE
      cases
    }


    def removeSubstOnly(makers: List[TreeMaker]) = makers filterNot (_.isInstanceOf[SubstOnlyTreeMaker])

    // a foldLeft to accumulate the localSubstitution left-to-right
    // it drops SubstOnly tree makers, since their only goal in life is to propagate substitutions to the next tree maker, which is fullfilled by propagateSubstitution
    def propagateSubstitution(treeMakers: List[TreeMaker], initial: Substitution): List[TreeMaker] = {
      var accumSubst: Substitution = initial
      treeMakers foreach { maker =>
        maker incorporateOuterSubstitution accumSubst
        accumSubst = maker.substitution
      }
      removeSubstOnly(treeMakers)
    }

    object SwitchablePattern { def unapply(pat: Tree) = pat match {
      case Literal(Constant((_: Byte ) | (_: Short) | (_: Int  ) | (_: Char ))) => true // TODO: Java 7 allows strings in switches
      case _                                                                    => false
    }}

    // def isSwitchable(cases: List[(List[TreeMaker], Tree)]): Boolean = {
    //   def isSwitchableTreeMaker(tm: TreeMaker) = tm match {
    //     case tm@EqualityTestTreeMaker(_, SwitchablePattern(), _) => true
    //     case SubstOnlyTreeMaker(_)                               => true
    //     case AlternativesTreeMaker(_, altss, _)                  => altss forall (_.forall(isSwitchableTreeMaker))
    //     case _                                                   => false
    //   }
    // }

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type): Option[Tree] = if (optimizingCodeGen) {
      def unfold(tms: List[TreeMaker], currLabel: Option[Symbol] = None, nextLabel: Option[Symbol] = None): List[CaseDef] = tms match {
        // constant
        case (EqualityTestTreeMaker(_, const@SwitchablePattern(), _)) :: (btm@BodyTreeMaker(body, _)) :: Nil => import CODE._
          @inline
          def substedBody  = btm.substitution(body)
          val labelledBody = currLabel match {
            case None          => substedBody // currLabel.isEmpty implies nextLabel.isEmpty
            case Some(myLabel) =>
              LabelDef(myLabel, Nil,
                nextLabel match {
                  case None       => substedBody
                  case Some(next) => ID(next) APPLY ()
                }
              )
          }
          List(CaseDef(const, EmptyTree, labelledBody))

        // alternatives
        case AlternativesTreeMaker(_, altss, _) :: bodyTm :: Nil => // assert(currLabel.isEmpty && nextLabel.isEmpty)
          val labels  = altss map { alts =>
            Some(freshSym(NoPosition, MethodType(Nil, pt), "$alt$") setFlag (METHOD | LABEL))
          }

          val caseDefs = (altss, labels, labels.tail :+ None).zipped.map { case (alts, currLabel, nextLabel) =>
            unfold(alts :+ bodyTm, currLabel, nextLabel)
          }

          if (caseDefs exists (_.isEmpty)) Nil
          else caseDefs.flatten

        case _ => Nil // failure
      }

      val caseDefs = cases map { makers =>
        removeSubstOnly(makers) match {
          // default case (don't move this to unfold, as it may only occur on the top level, not as an alternative -- well, except in degenerate matches)
          case (btm@BodyTreeMaker(body, _)) :: Nil =>
            List(CaseDef(Ident(nme.WILDCARD), EmptyTree, btm.substitution(body)))
          case nonTrivialMakers =>
            unfold(nonTrivialMakers)
        }
      }

      if (caseDefs exists (_.isEmpty)) None
      else { import CODE._
        val matcher = BLOCK(
          VAL(scrutSym) === scrut, // TODO: type test for switchable type if patterns allow switch but the scrutinee doesn't
          Match(REF(scrutSym), caseDefs.flatten) // match on scrutSym, not scrut to avoid duplicating scrut
        )

        // matcher filter (tree => tree.tpe == null) foreach println
        // treeBrowser browse matcher
        Some(matcher) // set type to avoid recursion in typedMatch
      }
    } else None

    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[List[TreeMaker]] =
      doCSE(prevBinder, doDCE(prevBinder, cases, pt), pt)

    // calls propagateSubstitution on the treemakers
    def combineCases(scrut: Tree, scrutSym: Symbol, casesRaw: List[List[TreeMaker]], pt: Type, owner: Symbol): Tree = fixerUpper(owner, scrut.pos){
      val casesUnOpt = casesRaw map (propagateSubstitution(_, EmptySubstitution)) // drops SubstOnlyTreeMakers, since their effect is now contained in the TreeMakers that follow them

      emitSwitch(scrut, scrutSym, casesUnOpt, pt).getOrElse{
        var toHoist = List[Tree]()
        val (matcher, hasDefault) =
          if (casesUnOpt nonEmpty) {
            // when specified, need to propagate pt explicitly (type inferencer can't handle it)
            val optPt =
              if (isFullyDefined(pt)) inMatchMonad(pt)
              else NoType

            // do this check on casesUnOpt, since DCE will eliminate trivial cases like `case _ =>`, even if they're the last one
            // exhaustivity and reachability must be checked before optimization as well
            val hasDefault = casesUnOpt.nonEmpty && {
              val nonTrivLast = casesUnOpt.last
              nonTrivLast.nonEmpty && nonTrivLast.head.isInstanceOf[BodyTreeMaker]
            }

            val cases =
              if (optimizingCodeGen) optimizeCases(scrutSym, casesUnOpt, pt)
              else casesUnOpt

            val combinedCases =
              cases.map(combineExtractors(_, pt)).reduceLeft(pmgen.typedOrElse(optPt))

            toHoist = (for (treeMakers <- cases; tm <- treeMakers; hoisted <- tm.treesToHoist) yield hoisted).toList

            (pmgen.fun(scrutSym, combinedCases), hasDefault)
          } else (pmgen.zero, false)

        val expr = pmgen.runOrElse(scrut, matcher, scrutSym.info, if (isFullyDefined(pt)) pt else NoType, hasDefault)
        if (toHoist isEmpty) expr
        else Block(toHoist, expr)
      }
    }

    // combineExtractors changes the current substitution's of the tree makers in `treeMakers`
    // requires propagateSubstitution(treeMakers) has been called
    def combineExtractors(treeMakers: List[TreeMaker], pt: Type): Tree =
      treeMakers.foldRight (EmptyTree: Tree) (_.chainBefore(_, pt))



    // TODO: do this during tree construction, but that will require tracking the current owner in treemakers
    // TODO: assign more fine-grained positions
    // fixes symbol nesting, assigns positions
    private def fixerUpper(origOwner: Symbol, pos: Position) = new Traverser {
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// substitution
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    object Substitution {
      def apply(from: Symbol, to: Tree) = new Substitution(List(from), List(to))
      // requires sameLength(from, to)
      def apply(from: List[Symbol], to: List[Tree]) =
        if (from nonEmpty) new Substitution(from, to) else EmptySubstitution
    }

    class Substitution(val from: List[Symbol], val to: List[Tree]) {
      def apply(tree: Tree): Tree = typedSubst(tree, from, to)

      // the substitution that chains `other` before `this` substitution
      // forall t: Tree. this(other(t)) == (this >> other)(t)
      def >>(other: Substitution): Substitution = {
        val (fromFiltered, toFiltered) = (from, to).zipped filter { (f, t) =>  !other.from.contains(f) }
        new Substitution(other.from ++ fromFiltered, other.to.map(apply) ++ toFiltered) // a quick benchmarking run indicates the `.map(apply)` is not too costly
      }
      override def toString = (from zip to) mkString("Substitution(", ", ", ")")
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
      def runOrElse(scrut: Tree, matcher: Tree, scrutTp: Type, resTp: Type, hasDefault: Boolean): Tree
      def flatMap(a: Tree, b: Tree): Tree
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, nextBinderTp: Type, next: Tree): Tree
      def flatMapGuard(cond: Tree, next: Tree): Tree
      def fun(arg: Symbol, body: Tree): Tree
      def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree
      def zero: Tree
      def one(res: Tree, bodyPt: Type, matchPt: Type): Tree
      def condOptimized(c: Tree, then: Tree): Tree
      def _equals(checker: Tree, binder: Symbol): Tree
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      def mkZero(tp: Type): Tree
    }

    def pmgen: AbsCodeGen
    def typed(tree: Tree, mode: Int, pt: Type): Tree // implemented in MatchTranslator
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// generate actual trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait MatchCodeGen extends TreeMakers {
    lazy val pmgen: CommonCodeGen with MatchingStrategyGen with MonadInstGen =
      if (optimizingCodeGen) (new CommonCodeGen with OptimizedCodeGen {})
      else (new CommonCodeGen with MatchingStrategyGen with MonadInstGen {})

    import CODE._

    trait MatchingStrategyGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      // methods in MatchingStrategy (the monad companion) -- used directly in translation
      def runOrElse(scrut: Tree, matcher: Tree, scrutTp: Type, resTp: Type, hasDefault: Boolean): Tree = genTypeApply(matchingStrategy DOT vpmName.runOrElse, scrutTp, resTp) APPLY (scrut) APPLY (matcher)  // matchingStrategy.runOrElse(scrut)(matcher)
      // *only* used to wrap the RHS of a body (isDefinedAt synthesis relies on this)
      def one(res: Tree, bodyPt: Type, matchPt: Type): Tree                       = (matchingStrategy DOT vpmName.one) (_asInstanceOf(res, bodyPt, force = true)) // matchingStrategy.one(res), like one, but blow this one away for isDefinedAt (since it's the RHS of a case)
      def zero: Tree                                                              = matchingStrategy DOT vpmName.zero                                // matchingStrategy.zero
      def guard(c: Tree, then: Tree, tp: Type): Tree                              = genTypeApply((matchingStrategy DOT vpmName.guard), repackExistential(tp)) APPLY (c, then) // matchingStrategy.guard[tp](c, then)
    }

    trait MonadInstGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      // methods in the monad instance -- used directly in translation
      def flatMap(a: Tree, b: Tree): Tree                             = (a DOT vpmName.flatMap)(b)
      def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree = (genTypeApply(thisCase DOT vpmName.orElse, pt)) APPLY (elseCase)

      // TODO: the trees generated by flatMapCond and flatMapGuard may need to be distinguishable by exhaustivity checking -- they aren't right now
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol,
                      nextBinderTp: Type, next: Tree): Tree           = flatMap(guard(cond, res, nextBinderTp), fun(nextBinder, next))
      def flatMapGuard(guardTree: Tree, next: Tree): Tree             = flatMapCond(guardTree, CODE.UNIT, freshSym(guardTree.pos, UnitClass.tpe), UnitClass.tpe, next)
    }

    // when we know we're targetting Option, do some inlining the optimizer won't do
    // `o.flatMap(f)` becomes `if(o == None) None else f(o.get)`, similarly for orElse and guard
    // this is a special instance of the advanced inlining optimization that takes a method call on
    // an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    // this trait overrides ALL of the methods of MatchingStrategyGen with MonadInstGen
    trait OptimizedCodeGen extends CommonCodeGen with MatchingStrategyGen with MonadInstGen {
      lazy val zeroSym        = freshSym(NoPosition, optionType(NothingClass.tpe), "zero")

      /** Inline runOrElse and get rid of Option allocations
       *
       * runOrElse(scrut: scrutTp)(matcher): resTp = matcher(scrut) getOrElse (throw new MatchError(x))
       * the matcher's optional result is encoded as a flag, keepGoing, where keepGoing == true encodes result.isEmpty,
       * if keepGoing is false, the result Some(x) of the naive translation is encoded as matchRes == x
       */
      @inline private def dontStore(tp: Type) = (tp.typeSymbol eq UnitClass) || (tp.typeSymbol eq NothingClass)
      lazy val keepGoing = freshSym(NoPosition, BooleanClass.tpe, "keepGoing") setFlag MUTABLE
      lazy val matchRes  = freshSym(NoPosition, AnyClass.tpe, "matchRes") setFlag MUTABLE
      override def runOrElse(scrut: Tree, matcher: Tree, scrutTp: Type, resTp: Type, hasDefault: Boolean) = matcher match {
        case Function(List(x: ValDef), body) =>
          matchRes.info = if (resTp ne NoType) resTp.widen else AnyClass.tpe // we don't always know resTp, and it might be AnyVal, in which case we can't assign NULL
          if (dontStore(resTp)) matchRes resetFlag MUTABLE  // don't assign to Unit-typed var's, in fact, make it a val -- conveniently also works around SI-5245
          BLOCK(
            VAL(zeroSym)   === REF(NoneModule), // TODO: can we just get rid of explicitly emitted zero? don't know how to do that as a local rewrite...
            VAL(x.symbol)  === scrut, // reuse the symbol of the function's argument to avoid creating a fresh one and substituting it for x.symbol in body -- the owner structure is repaired by fixerUpper
            VAL(matchRes)  === mkZero(matchRes.info), // must cast to deal with GADT typing, hence the private mkZero above
            VAL(keepGoing) === TRUE,
            body,
            if(hasDefault) REF(matchRes)
            else (IF (REF(keepGoing)) THEN MATCHERROR(REF(x.symbol)) ELSE REF(matchRes))
          )
      }

      // only used to wrap the RHS of a body
      override def one(res: Tree, bodyPt: Type, matchPt: Type): Tree = {
        BLOCK(
          if (dontStore(matchPt)) res // runOrElse hasn't been called yet, so matchRes.isMutable is irrelevant, also, tp may be a subtype of resTp used in runOrElse...
          else (REF(matchRes) === res), // _asInstanceOf(res, tp.widen, force = true)
          REF(keepGoing) === FALSE,
          zero // to have a nice lub for lubs -- otherwise we'll get a boxed unit here -- TODO: get rid of all those dangling else zero's
        )
      }

      override def zero: Tree = REF(zeroSym)

      // guard is only used by flatMapCond and flatMapGuard, which are overridden
      override def guard(c: Tree, then: Tree, tp: Type): Tree = throw new NotImplementedError("guard is never called by optimizing codegen")

      override def flatMap(opt: Tree, fun: Tree): Tree = fun match {
        case Function(List(x: ValDef), body) =>
          val tp      = inMatchMonad(x.symbol.tpe)
          val vs      = freshSym(opt.pos, tp, "o")
          val isEmpty = tp member vpmName.isEmpty
          val get     = tp member vpmName.get
          val v       = VAL(vs) === opt

          BLOCK(
            v,
            IF (vs DOT isEmpty) THEN zero ELSE typedSubst(body, List(x.symbol), List(vs DOT get)) // must be isEmpty and get as we don't control the target of the call (could be the result of a user-defined extractor)
          )
        case _ => println("huh?")
          (opt DOT vpmName.flatMap)(fun)
      }

      override def typedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree = {
        BLOCK(
          thisCase,
          IF (REF(keepGoing)) THEN elseCase ELSE zero // leave trailing zero for now, otherwise typer adds () anyway
        )
      }

      override def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, nextBinderTp: Type, next: Tree): Tree =
        IF (cond) THEN BLOCK(
          VAL(nextBinder) === res,
          next
        ) ELSE zero

      override def flatMapGuard(guardTree: Tree, next: Tree): Tree =
        IF (guardTree) THEN next ELSE zero
    }

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
      val one       = "one".toTermName
      val drop      = "drop".toTermName
      val flatMap   = "flatMap".toTermName
      val get       = "get".toTermName
      val guard     = "guard".toTermName
      val isEmpty   = "isEmpty".toTermName
      val orElse    = "orElse".toTermName
      val outer     = "<outer>".toTermName
      val runOrElse = "runOrElse".toTermName
      val zero      = "zero".toTermName

      def counted(str: String, i: Int) = (str+i).toTermName
      def tupleIndex(i: Int) = ("_"+i).toTermName
    }


    def typesConform(tp: Type, pt: Type) = ((tp eq pt) || (tp <:< pt))

    trait CommonCodeGen extends AbsCodeGen { self: CommonCodeGen with MatchingStrategyGen with MonadInstGen =>
      def fun(arg: Symbol, body: Tree): Tree           = Function(List(ValDef(arg)), body)
      def genTypeApply(tfun: Tree, args: Type*): Tree  = if(args contains NoType) tfun else TypeApply(tfun, args.toList map TypeTree)
      def tupleSel(binder: Symbol)(i: Int): Tree       = (REF(binder) DOT vpmName.tupleIndex(i)) // make tree that accesses the i'th component of the tuple referenced by binder
      def index(tgt: Tree)(i: Int): Tree               = tgt APPLY (LIT(i))
      def drop(tgt: Tree)(n: Int): Tree                = (tgt DOT vpmName.drop) (LIT(n))
      def _equals(checker: Tree, binder: Symbol): Tree = checker MEMBER_== REF(binder)          // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def and(a: Tree, b: Tree): Tree                  = a AND b
      def condOptimized(c: Tree, then: Tree): Tree     = IF (c) THEN then ELSE zero

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

      // duplicated out of frustration with cast generation
      def mkZero(tp: Type): Tree = {
        tp.typeSymbol match {
          case UnitClass    => Literal(Constant())
          case BooleanClass => Literal(Constant(false))
          case FloatClass   => Literal(Constant(0.0f))
          case DoubleClass  => Literal(Constant(0.0d))
          case ByteClass    => Literal(Constant(0.toByte))
          case ShortClass   => Literal(Constant(0.toShort))
          case IntClass     => Literal(Constant(0))
          case LongClass    => Literal(Constant(0L))
          case CharClass    => Literal(Constant(0.toChar))
          case _            => gen.mkAsInstanceOf(Literal(Constant(null)), tp, any = true, wrapInApply = false) // the magic incantation is true/false here
        }
      }
    }

    def matchingStrategy: Tree
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

    //   def approximateTreeMaker(tm: TreeMaker): List[Test] = tm match {
    //     case ExtractorTreeMaker(extractor, _, _) => HavocTest
    //     case FilteredExtractorTreeMaker(extractor, lenGuard, _, _) => HavocTest
    //     case ProductExtractorTreeMaker(testedBinder, lenGuard, _) => TopTest // TODO: (testedBinder ne null) and lenGuard
    //
    //     // cond = typeTest(prevBinder, nextBinderTp)
    //     // res  = pmgen._asInstanceOf(prevBinder, nextBinderTp)
    //     case TypeTestTreeMaker(testedBinder, pt, _) =>
    //
    //     // cond = typeAndEqualityTest(patBinder, pt)
    //     // res  = pmgen._asInstanceOf(patBinder, nextBinderTp)
    //     case TypeAndEqualityTestTreeMaker(_, testedBinder, pt, _) =>
    //
    //     // cond = pmgen._equals(patTree, prevBinder)
    //     // res  = CODE.REF(prevBinder)
    //     case EqualityTestTreeMaker(testedBinder, rhs, _) =>
    //
    //     case AlternativesTreeMaker(_, alts: *) =>
    //
    //     case GuardTreeMaker(guardTree) =>
    //   }

    // // TODO: it's not exactly sound to represent an unapply-call by its symbol... also need to consider the prefix, like the outer-test (can this be captured as the path to this test?)
    // type ExtractorRepr = Symbol
    //
    // // TODO: we're undoing tree-construction that we ourselves performed earlier -- how about not-doing so we don't have to undo?
    // private def findBinderArgOfApply(extractor: Tree, unappSym: Symbol): Symbol = {
    //   class CollectTreeTraverser[T](pf: PartialFunction[Tree => T]) extends Traverser {
    //     val hits = new ListBuffer[T]
    //     override def traverse(t: Tree) {
    //       if (pf.isDefinedAt(t)) hits += pf(t)
    //       super.traverse(t)
    //     }
    //   }
    //   val trav = new CollectTreeTraverser{ case Apply(unapp, List(arg)) if unapp.symbol eq unappSym => arg.symbol}
    //   trav.traverse(extractor)
    //   trav.hits.headOption getOrElse NoSymbol
    // }
