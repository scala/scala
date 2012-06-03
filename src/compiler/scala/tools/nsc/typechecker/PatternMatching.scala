/* NSC -- new Scala compiler
 *
 * Copyright 2012 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package typechecker

import symtab._
import Flags.{MUTABLE, METHOD, LABEL, SYNTHETIC}
import language.postfixOps
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.tools.nsc.util.Statistics

/** Translate pattern matching.
  *
  * Either into optimized if/then/else's,
  * or virtualized as method calls (these methods form a zero-plus monad), similar in spirit to how for-comprehensions are compiled.
  *
  * For each case, express all patterns as extractor calls, guards as 0-ary extractors, and sequence them using `flatMap`
  * (lifting the body of the case into the monad using `one`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `zero`).
  *
  * TODO:
  *  - use TypeTags for type testing
  *  - DCE (on irrefutable patterns)
  *  - update spec and double check it's implemented correctly (see TODO's)
  *
  * (longer-term) TODO:
  *  - user-defined unapplyProd
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  *  - recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  */
trait PatternMatching extends Transform with TypingTransformers with ast.TreeDSL {   // self: Analyzer =>
  import Statistics._

  val global: Global               // need to repeat here because otherwise last mixin defines global as
                                   // SymbolTable. If we had DOT this would not be an issue
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  val phaseName: String = "patmat"

  def patmatDebug(msg: String) = println(msg)

  def newTransformer(unit: CompilationUnit): Transformer =
    if (opt.virtPatmat) new MatchTransformer(unit)
    else noopTransformer

  // duplicated from CPSUtils (avoid dependency from compiler -> cps plugin...)
  private lazy val MarkerCPSAdaptPlus  = definitions.getClassIfDefined("scala.util.continuations.cpsPlus")
  private lazy val MarkerCPSAdaptMinus = definitions.getClassIfDefined("scala.util.continuations.cpsMinus")
  private lazy val MarkerCPSSynth      = definitions.getClassIfDefined("scala.util.continuations.cpsSynth")
  private lazy val stripTriggerCPSAnns = List(MarkerCPSSynth, MarkerCPSAdaptMinus, MarkerCPSAdaptPlus)
  private lazy val MarkerCPSTypes      = definitions.getClassIfDefined("scala.util.continuations.cpsParam")
  private lazy val strippedCPSAnns     = MarkerCPSTypes :: stripTriggerCPSAnns
  private def removeCPSAdaptAnnotations(tp: Type) = tp filterAnnotations (ann => !(strippedCPSAnns exists (ann matches _)))

  class MatchTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case Match(sel, cases) =>
        val origTp = tree.tpe
        // setType origTp intended for CPS -- TODO: is it necessary?
        localTyper.typed(translator.translateMatch(treeCopy.Match(tree, transform(sel), transformTrees(cases).asInstanceOf[List[CaseDef]]))) setType origTp
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), translator.translateTry(transformTrees(catches).asInstanceOf[List[CaseDef]], tree.tpe, tree.pos), transform(finalizer))
      case _ => super.transform(tree)
    }

    def translator: MatchTranslation with CodegenCore = {
      new OptimizingMatchTranslator(localTyper)
    }
  }

  import definitions._
  import analyzer._ //Typer

  val SYNTH_CASE = Flags.CASE | SYNTHETIC

  case class DefaultOverrideMatchAttachment(default: Tree)

  object vpmName {
    val one       = newTermName("one")
    val drop      = newTermName("drop")
    val flatMap   = newTermName("flatMap")
    val get       = newTermName("get")
    val guard     = newTermName("guard")
    val isEmpty   = newTermName("isEmpty")
    val orElse    = newTermName("orElse")
    val outer     = newTermName("<outer>")
    val runOrElse = newTermName("runOrElse")
    val zero      = newTermName("zero")
    val _match    = newTermName("__match") // don't call the val __match, since that will trigger virtual pattern matching...

    def counted(str: String, i: Int) = newTermName(str+i)
  }

  class PureMatchTranslator(val typer: Typer, val matchStrategy: Tree) extends MatchTranslation with TreeMakers with PureCodegen
  class OptimizingMatchTranslator(val typer: Typer)                    extends MatchTranslation with TreeMakers with MatchOptimizations

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// talking to userland
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Interface with user-defined match monad?
   * if there's a `__match` in scope, we use this as the match strategy, assuming it conforms to MatchStrategy as defined below:

       type Matcher[P[_], M[+_], A] = {
         def flatMap[B](f: P[A] => M[B]): M[B]
         def orElse[B >: A](alternative: => M[B]): M[B]
       }

       abstract class MatchStrategy[P[_], M[+_]] {
         // runs the matcher on the given input
         def runOrElse[T, U](in: P[T])(matcher: P[T] => M[U]): P[U]

         def zero: M[Nothing]
         def one[T](x: P[T]): M[T]
         def guard[T](cond: P[Boolean], then: => P[T]): M[T]
         def isSuccess[T, U](x: P[T])(f: P[T] => M[U]): P[Boolean] // used for isDefinedAt
       }

   * P and M are derived from one's signature (`def one[T](x: P[T]): M[T]`)


   * if no `__match` is found, we assume the following implementation (and generate optimized code accordingly)

       object __match extends MatchStrategy[({type Id[x] = x})#Id, Option] {
         def zero = None
         def one[T](x: T) = Some(x)
         // NOTE: guard's return type must be of the shape M[T], where M is the monad in which the pattern match should be interpreted
         def guard[T](cond: Boolean, then: => T): Option[T] = if(cond) Some(then) else None
         def runOrElse[T, U](x: T)(f: T => Option[U]): U = f(x) getOrElse (throw new MatchError(x))
         def isSuccess[T, U](x: T)(f: T => Option[U]): Boolean = !f(x).isEmpty
       }

   */
  trait MatchMonadInterface {
    val typer: Typer
    val matchOwner = typer.context.owner

    def inMatchMonad(tp: Type): Type
    def pureType(tp: Type): Type
    final def matchMonadResult(tp: Type): Type =
      tp.baseType(matchMonadSym).typeArgs match {
        case arg :: Nil => arg
        case _ => ErrorType
      }

    protected def matchMonadSym: Symbol
  }

  trait MatchTranslation extends MatchMonadInterface { self: TreeMakers with CodegenCore =>
    import typer.{typed, context, silent, reallyExists}
    // import typer.infer.containsUnchecked

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

      // we don't transform after uncurry
      // (that would require more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      if(phase.id >= currentRun.uncurryPhase.id) debugwarn("running translateMatch at "+ phase +" on "+ selector +" match "+ cases)
      // patmatDebug("translating "+ cases.mkString("{", "\n", "}"))

      def repeatedToSeq(tp: Type): Type = (tp baseType RepeatedParamClass) match {
        case TypeRef(_, RepeatedParamClass, arg :: Nil) => seqType(arg)
        case _                                          => tp
      }

      val start = startTimer(patmatNanos)

      val selectorTp = repeatedToSeq(elimAnonymousClass(selector.tpe.widen.withoutAnnotations))

      val origPt  = match_.tpe
      // when one of the internal cps-type-state annotations is present, strip all CPS annotations
      // a cps-type-state-annotated type makes no sense as an expected type (matchX.tpe is used as pt in translateMatch)
      // (only test availability of MarkerCPSAdaptPlus assuming they are either all available or none of them are)
      val ptUnCPS =
        if (MarkerCPSAdaptPlus != NoSymbol && (stripTriggerCPSAnns exists origPt.hasAnnotation))
          removeCPSAdaptAnnotations(origPt)
        else origPt

      // we've packed the type for each case in typedMatch so that if all cases have the same existential case, we get a clean lub
      // here, we should open up the existential again
      // relevant test cases: pos/existentials-harmful.scala, pos/gadt-gilles.scala, pos/t2683.scala, pos/virtpatmat_exist4.scala
      // TODO: fix skolemizeExistential (it should preserve annotations, right?)
      val pt = repeatedToSeq(ptUnCPS.skolemizeExistential(context.owner, context.tree) withAnnotations ptUnCPS.annotations)

      // the alternative to attaching the default case override would be to simply
      // append the default to the list of cases and suppress the unreachable case error that may arise (once we detect that...)
      val matchFailGenOverride = match_ firstAttachment {case DefaultOverrideMatchAttachment(default) => ((scrut: Tree) => default)}

      val selectorSym  = freshSym(selector.pos, pureType(selectorTp)) setFlag SYNTH_CASE
      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      val combined = combineCases(selector, selectorSym, cases map translateCase(selectorSym, pt), pt, matchOwner, matchFailGenOverride)

      stopTimer(patmatNanos, start)
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
            val caseScrutSym = freshSym(pos, pureType(ThrowableClass.tpe))
            (caseScrutSym, propagateSubstitution(translateCase(caseScrutSym, pt)(caseDef), EmptySubstitution))
          }

          for(cases <- emitTypeSwitch(bindersAndCases, pt).toList;
              if cases forall treeInfo.isCatchCase; // must check again, since it's not guaranteed -- TODO: can we eliminate this? e.g., a type test could test for a trait or a non-trivial prefix, which are not handled by the back-end
              cse <- cases) yield fixerUpper(matchOwner, pos)(cse).asInstanceOf[CaseDef]
        }

        val catches = if (swatches.nonEmpty) swatches else {
          val scrutSym = freshSym(pos, pureType(ThrowableClass.tpe))
          val casesNoSubstOnly = caseDefs map { caseDef => (propagateSubstitution(translateCase(scrutSym, pt)(caseDef), EmptySubstitution))}

          val exSym = freshSym(pos, pureType(ThrowableClass.tpe), "ex")

          List(
              atPos(pos) {
                CaseDef(
                  Bind(exSym, Ident(nme.WILDCARD)), // TODO: does this need fixing upping?
                  EmptyTree,
                  combineCasesNoSubstOnly(CODE.REF(exSym), scrutSym, casesNoSubstOnly, pt, matchOwner, Some(scrut => Throw(CODE.REF(exSym))))
                )
              })
        }

        typer.typedCases(catches, ThrowableClass.tpe, WildcardType)
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
        if (!extractor.isTyped) ErrorUtils.issueNormalTypeError(patTree, "Could not typecheck extractor call: "+ extractor)(context)
        // if (extractor.resultInMonad == ErrorType) throw new TypeError(pos, "Unsupported extractor type: "+ extractor.tpe)

        // must use type `tp`, which is provided by extractor's result, not the type expected by binder,
        // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
        // (it will later result in a type test when `tp` is not a subtype of `b.info`)
        // TODO: can we simplify this, together with the Bound case?
        (extractor.subPatBinders, extractor.subPatTypes).zipped foreach { case (b, tp) => b setInfo tp } // patmatDebug("changing "+ b +" : "+ b.info +" -> "+ tp);

        // patmatDebug("translateExtractorPattern checking parameter type: "+ (patBinder, patBinder.info.widen, extractor.paramType, patBinder.info.widen <:< extractor.paramType))
        // example check: List[Int] <:< ::[Int]
        // TODO: extractor.paramType may contain unbound type params (run/t2800, run/t3530)
        val (typeTestTreeMaker, patBinderOrCasted) =
          if (needsTypeTest(patBinder.info.widen, extractor.paramType)) {
            // chain a type-testing extractor before the actual extractor call
            // it tests the type, checks the outer pointer and casts to the expected type
            // TODO: the outer check is mandated by the spec for case classes, but we do it for user-defined unapplies as well [SPEC]
            // (the prefix of the argument passed to the unapply must equal the prefix of the type of the binder)
            val treeMaker = TypeTestTreeMaker(patBinder, patBinder, extractor.paramType, extractor.paramType)(pos, extractorArgTypeTest = true)
            (List(treeMaker), treeMaker.nextBinder)
          } else (Nil, patBinder)

        withSubPats(typeTestTreeMaker :+ extractor.treeMaker(patBinderOrCasted, pos), extractor.subBindersAndPatterns: _*)
      }


      object MaybeBoundTyped {
        /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
          * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these).
          * The returned type is the one inferred by inferTypedPattern (`owntype`)
          *
          * @arg patBinder  symbol used to refer to the result of the previous pattern's extractor (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
        def unapply(tree: Tree): Option[(Symbol, Type)] = tree match {
          // the Ident subpattern can be ignored, subpatBinder or patBinder tell us all we need to know about it
          case Bound(subpatBinder, typed@Typed(Ident(_), tpt)) if typed.tpe ne null => Some((subpatBinder, typed.tpe))
          case Bind(_, typed@Typed(Ident(_), tpt))             if typed.tpe ne null => Some((patBinder, typed.tpe))
          case Typed(Ident(_), tpt)                            if tree.tpe ne null  => Some((patBinder, tree.tpe))
          case _  => None
        }
      }

      val (treeMakers, subpats) = patTree match {
        // skip wildcard trees -- no point in checking them
        case WildcardPattern() => noFurtherSubPats()
        case UnApply(unfun, args) =>
          // TODO: check unargs == args
          // patmatDebug("unfun: "+ (unfun.tpe, unfun.symbol.ownerChain, unfun.symbol.info, patBinder.info))
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
            ErrorUtils.issueNormalTypeError(patTree, "Could not find unapply member for "+ fun +" with args "+ args)(context)
            noFurtherSubPats()
          }

        /** A typed pattern x : T consists of a pattern variable x and a type pattern T.
            The type of x is the type pattern T, where each type variable and wildcard is replaced by a fresh, unknown type.
            This pattern matches any value matched by the type pattern T (§8.2); it binds the variable name to that value.
        **/
        // must treat Typed and Bind together -- we need to know the patBinder of the Bind pattern to get at the actual type
        case MaybeBoundTyped(subPatBinder, pt) =>
          // a typed pattern never has any subtrees
          noFurtherSubPats(TypeTestTreeMaker(subPatBinder, patBinder, pt, glb(List(patBinder.info.widen, pt)).normalize)(pos))

        /** A pattern binder x@p consists of a pattern variable x and a pattern p.
            The type of the variable x is the static type T of the pattern p.
            This pattern matches any value v matched by the pattern p,
            provided the run-time type of v is also an instance of T,  <-- TODO! https://issues.scala-lang.org/browse/SI-1503
            and it binds the variable name to that value.
        **/
        case Bound(subpatBinder, p)          =>
          // replace subpatBinder by patBinder (as if the Bind was not there)
          withSubPats(List(SubstOnlyTreeMaker(subpatBinder, patBinder)),
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
          // patmatDebug("WARNING: Bind tree with unbound symbol "+ patTree)
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

    // TODO: 1) if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (which is now done by codegen.one),
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
              case SilentResultValue(extractorCall) => extractorCall // if !extractorCall.containsError()
              case _ =>
                // this fails to resolve overloading properly...
                // Apply(typedOperator(Select(orig, extractor)), List(Ident(nme.SELECTOR_DUMMY))) // no need to set the type of the dummy arg, it will be replaced anyway

                // patmatDebug("funtpe after = "+ fun.tpe.finalResultType)
                // patmatDebug("orig: "+(orig, orig.tpe))
                val tgt = typed(orig, EXPRmode | QUALmode | POLYmode, HasMember(extractor.name)) // can't specify fun.tpe.finalResultType as the type for the extractor's arg,
                // as it may have been inferred incorrectly (see t602, where it's  com.mosol.sl.Span[Any], instead of  com.mosol.sl.Span[?K])
                // patmatDebug("tgt = "+ (tgt, tgt.tpe))
                val oper = typed(Select(tgt, extractor.name), EXPRmode | FUNmode | POLYmode | TAPPmode, WildcardType)
                // patmatDebug("oper: "+ (oper, oper.tpe))
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
      protected def tupleSel(binder: Symbol)(i: Int): Tree = codegen.tupleSel(binder)(i)

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require isSeq
      protected def subPatRefsSeq(binder: Symbol): List[Tree] = {
        val indexingIndices   = (0 to (lastIndexingBinder-firstIndexingBinder))
        val nbIndexingIndices = indexingIndices.length

        // this error-condition has already been checked by checkStarPatOK:
        //   if(isSeq) assert(firstIndexingBinder + nbIndexingIndices + (if(lastIsStar) 1 else 0) == nbSubPats, "(resultInMonad, ts, subPatTypes, subPats)= "+(resultInMonad, ts, subPatTypes, subPats))
        // there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
        (((1 to firstIndexingBinder) map tupleSel(binder)) ++
        // then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
        (indexingIndices map codegen.index(seqTree(binder))) ++
        // the last one -- if the last subpattern is a sequence wildcard: drop the prefix (indexed by the refs on the line above), return the remainder
        (if(!lastIsStar) Nil else List(
          if(nbIndexingIndices == 0) seqTree(binder)
          else codegen.drop(seqTree(binder))(nbIndexingIndices)))).toList
      }

      // the trees that select the subpatterns on the extractor's result, referenced by `binder`
      // require (nbSubPats > 0 && (!lastIsStar || isSeq))
      protected def subPatRefs(binder: Symbol): List[Tree] =
        if (nbSubPats == 0) Nil
        else if (isSeq) subPatRefsSeq(binder)
        else ((1 to nbSubPats) map tupleSel(binder)).toList

      protected def lengthGuard(binder: Symbol): Option[Tree] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        checkedLength map { expectedLength => import CODE._
          // `binder.lengthCompare(expectedLength)`
          def checkExpectedLength = (seqTree(binder) DOT seqLenCmp)(LIT(expectedLength))

          // the comparison to perform
          // when the last subpattern is a wildcard-star the expectedLength is but a lower bound
          // (otherwise equality is required)
          def compareOp: (Tree, Tree) => Tree =
            if (lastIsStar)  _ INT_>= _
            else             _ INT_== _

          // `if (binder != null && $checkExpectedLength [== | >=] 0) then else zero`
          (seqTree(binder) ANY_!= NULL) AND compareOp(checkExpectedLength, ZERO)
        }

      def checkedLength: Option[Int] =
        // no need to check unless it's an unapplySeq and the minimal length is non-trivially satisfied
        if (!isSeq || (expectedLength < minLenToCheck)) None
        else Some(expectedLength)

    }

    // TODO: to be called when there's a def unapplyProd(x: T): U
    // U must have N members _1,..., _N -- the _i are type checked, call their type Ti,
    //
    // for now only used for case classes -- pretending there's an unapplyProd that's the identity (and don't call it)
    class ExtractorCallProd(fun: Tree, args: List[Tree]) extends ExtractorCall(args) {
      // TODO: fix the illegal type bound in pos/t602 -- type inference messes up before we get here:
      /*override def equals(x$1: Any): Boolean = ...
             val o5: Option[com.mosol.sl.Span[Any]] =  // Span[Any] --> Any is not a legal type argument for Span!
      */
      // private val orig            = fun match {case tpt: TypeTree => tpt.original case _ => fun}
      // private val origExtractorTp = unapplyMember(orig.symbol.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe).tpe
      // private val extractorTp     = if (wellKinded(fun.tpe)) fun.tpe else existentialAbstraction(origExtractorTp.typeParams, origExtractorTp.resultType)
      // patmatDebug("ExtractorCallProd: "+ (fun.tpe, existentialAbstraction(origExtractorTp.typeParams, origExtractorTp.resultType)))
      // patmatDebug("ExtractorCallProd: "+ (fun.tpe, args map (_.tpe)))
      private def constructorTp = fun.tpe

      def isTyped    = fun.isTyped

      // to which type should the previous binder be casted?
      def paramType  = constructorTp.finalResultType

      def isSeq: Boolean = rawSubPatTypes.nonEmpty && isRepeatedParamType(rawSubPatTypes.last)
      protected def rawSubPatTypes = constructorTp.paramTypes

      // binder has type paramType
      def treeMaker(binder: Symbol, pos: Position): TreeMaker = {
        // checks binder ne null before chaining to the next extractor
        ProductExtractorTreeMaker(binder, lengthGuard(binder), Substitution(subPatBinders, subPatRefs(binder)))
      }

      // reference the (i-1)th case accessor if it exists, otherwise the (i-1)th tuple component
      override protected def tupleSel(binder: Symbol)(i: Int): Tree = { import CODE._
        // caseFieldAccessors is messed up after typers (reversed, names mangled for non-public fields)
        // TODO: figure out why...
        val accessors = binder.caseFieldAccessors
        // luckily, the constrParamAccessors are still sorted properly, so sort the field-accessors using them
        // (need to undo name-mangling, including the sneaky trailing whitespace)
        val constrParamAccessors = binder.constrParamAccessors

        def indexInCPA(acc: Symbol) =
          constrParamAccessors indexWhere { orig =>
            // patmatDebug("compare: "+ (orig, acc, orig.name, acc.name, (acc.name == orig.name), (acc.name startsWith (orig.name append "$"))))
            val origName  = orig.name.toString.trim
            val accName = acc.name.toString.trim
            (accName == origName) || (accName startsWith (origName + "$"))
          }

        // patmatDebug("caseFieldAccessors: "+ (accessors, binder.caseFieldAccessors map indexInCPA))
        // patmatDebug("constrParamAccessors: "+ constrParamAccessors)

        val accessorsSorted = accessors sortBy indexInCPA
        if (accessorsSorted isDefinedAt (i-1)) REF(binder) DOT accessorsSorted(i-1)
        else codegen.tupleSel(binder)(i) // this won't type check for case classes, as they do not inherit ProductN
      }

      override def toString(): String = "case class "+ (if (constructorTp eq null) fun else paramType.typeSymbol) +" with arguments "+ args
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
        val binder         = freshSym(pos, pureType(resultInMonad)) // can't simplify this when subPatBinders.isEmpty, since UnitClass.tpe is definitely wrong when isSeq, and resultInMonad should always be correct since it comes directly from the extractor's result type
        ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder, Substitution(subPatBinders, subPatRefs(binder)))(resultType.typeSymbol == BooleanClass, checkedLength, patBinderOrCasted)
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
        else matchMonadResult(resultType)
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
// substitution
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TypedSubstitution extends MatchMonadInterface {
    object Substitution {
      def apply(from: Symbol, to: Tree) = new Substitution(List(from), List(to))
      // requires sameLength(from, to)
      def apply(from: List[Symbol], to: List[Tree]) =
        if (from nonEmpty) new Substitution(from, to) else EmptySubstitution
    }

    class Substitution(val from: List[Symbol], val to: List[Tree]) {
      // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
      // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
      def apply(tree: Tree): Tree = {
        // according to -Ystatistics 10% of translateMatch's time is spent in this method...
        // since about half of the typedSubst's end up being no-ops, the check below shaves off 5% of the time spent in typedSubst
        if (!tree.exists { case i@Ident(_) => from contains i.symbol case _ => false}) tree
        else (new Transformer {
          @inline private def typedIfOrigTyped(to: Tree, origTp: Type): Tree =
            if (origTp == null || origTp == NoType) to
            // important: only type when actually substing and when original tree was typed
            // (don't need to use origTp as the expected type, though, and can't always do this anyway due to unknown type params stemming from polymorphic extractors)
            else typer.typed(to, EXPRmode, WildcardType)

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


      // the substitution that chains `other` before `this` substitution
      // forall t: Tree. this(other(t)) == (this >> other)(t)
      def >>(other: Substitution): Substitution = {
        val (fromFiltered, toFiltered) = (from, to).zipped filter { (f, t) =>  !other.from.contains(f) }
        new Substitution(other.from ++ fromFiltered, other.to.map(apply) ++ toFiltered) // a quick benchmarking run indicates the `.map(apply)` is not too costly
      }
      override def toString = (from.map(_.name) zip to) mkString("Substitution(", ", ", ")")
    }

    object EmptySubstitution extends Substitution(Nil, Nil) {
      override def apply(tree: Tree): Tree = tree
      override def >>(other: Substitution): Substitution = other
    }
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// the making of the trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TreeMakers extends TypedSubstitution { self: CodegenCore =>
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, unchecked: Boolean): (List[List[TreeMaker]], List[Tree]) =
      (cases, Nil)

    def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree]): Option[Tree] =
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
          // patmatDebug("BUG: incorporateOuterSubstitution called more than once for "+ (this, currSub, outerSubst))
          Thread.dumpStack()
        }
        else currSub = outerSubst >> substitution
      }
      private[this] var currSub: Substitution = null

      // build Tree that chains `next` after the current extractor
      def chainBefore(next: Tree)(casegen: Casegen): Tree
    }

    trait NoNewBinders extends TreeMaker {
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
      override def toString = "B"+(body, matchPt)
    }

    case class SubstOnlyTreeMaker(prevBinder: Symbol, nextBinder: Symbol) extends TreeMaker {
      val pos = NoPosition

      val localSubstitution = Substitution(prevBinder, CODE.REF(nextBinder))
      def chainBefore(next: Tree)(casegen: Casegen): Tree = substitution(next)
      override def toString = "S"+ localSubstitution
    }

    abstract class FunTreeMaker extends TreeMaker {
      val nextBinder: Symbol
      def pos = nextBinder.pos
    }

    abstract class CondTreeMaker extends FunTreeMaker {
      val prevBinder: Symbol
      val nextBinderTp: Type
      val cond: Tree
      val res: Tree

      lazy val nextBinder = freshSym(pos, nextBinderTp)
      lazy val localSubstitution = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))

      def chainBefore(next: Tree)(casegen: Casegen): Tree =
        atPos(pos)(casegen.flatMapCond(cond, res, nextBinder, substitution(next)))
    }

    /**
     * Make a TreeMaker that will result in an extractor call specified by `extractor`
     * the next TreeMaker (here, we don't know which it'll be) is chained after this one by flatMap'ing
     * a function with binder `nextBinder` over our extractor's result
     * the function's body is determined by the next TreeMaker
     * in this function's body, and all the subsequent ones, references to the symbols in `from` will be replaced by the corresponding tree in `to`
     */
    case class ExtractorTreeMaker(extractor: Tree, extraCond: Option[Tree], nextBinder: Symbol, localSubstitution: Substitution)(extractorReturnsBoolean: Boolean, val checkedLength: Option[Int], val prevBinder: Symbol) extends FunTreeMaker {
      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val condAndNext = extraCond map (casegen.ifThenElseZero(_, next)) getOrElse next
        atPos(extractor.pos)(
          if (extractorReturnsBoolean) casegen.flatMapCond(extractor, CODE.UNIT, nextBinder, substitution(condAndNext))
          else casegen.flatMap(extractor, nextBinder, substitution(condAndNext))
        )
      }

      override def toString = "X"+(extractor, nextBinder.name)
    }

    // TODO: allow user-defined unapplyProduct
    case class ProductExtractorTreeMaker(prevBinder: Symbol, extraCond: Option[Tree], localSubstitution: Substitution) extends FunTreeMaker { import CODE._
      val nextBinder = prevBinder // just passing through
      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val nullCheck = REF(prevBinder) OBJ_NE NULL
        val cond = extraCond map (nullCheck AND _) getOrElse nullCheck
        casegen.ifThenElseZero(cond, substitution(next))
      }

      override def toString = "P"+(prevBinder.name,  extraCond getOrElse "", localSubstitution)
    }

    // typetag-based tests are inserted by the type checker
    def needsTypeTest(tp: Type, pt: Type): Boolean = !(tp <:< pt)

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
      }

      object treeCondStrategy extends TypeTestCondStrategy { import CODE._
        type Result = Tree

        def and(a: Result, b: Result): Result                = a AND b
        def typeTest(testedBinder: Symbol, expectedTp: Type) = codegen._isInstanceOf(testedBinder, expectedTp)
        def nonNullTest(testedBinder: Symbol)                = REF(testedBinder) OBJ_NE NULL
        def equalsTest(pat: Tree, testedBinder: Symbol)      = codegen._equals(pat, testedBinder)
        def eqTest(pat: Tree, testedBinder: Symbol)          = REF(testedBinder) OBJ_EQ pat

        def outerTest(testedBinder: Symbol, expectedTp: Type): Tree = {
          val expectedOuter = expectedTp.prefix match {
            case ThisType(clazz)  => THIS(clazz)
            case pre              => REF(pre.prefix, pre.termSymbol)
          }

          // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
          // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
          val outer = expectedTp.typeSymbol.newMethod(vpmName.outer) setInfo expectedTp.prefix setFlag SYNTHETIC

          (Select(codegen._asInstanceOf(testedBinder, expectedTp), outer)) OBJ_EQ expectedOuter
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
      // patmatDebug("TTTM"+(prevBinder, extractorArgTypeTest, testedBinder, expectedTp, nextBinderTp))

      lazy val outerTestNeeded = (
          !((expectedTp.prefix eq NoPrefix) || expectedTp.prefix.typeSymbol.isPackageClass)
        && needsOuterTest(expectedTp, testedBinder.info, matchOwner))

      // the logic to generate the run-time test that follows from the fact that
      // a `prevBinder` is expected to have type `expectedTp`
      // the actual tree-generation logic is factored out, since the analyses generate Cond(ition)s rather than Trees
      // TODO: `null match { x : T }` will yield a check that (indirectly) tests whether `null ne null`
      // don't bother (so that we don't end up with the warning "comparing values of types Null and Null using `ne' will always yield false")
      def renderCondition(cs: TypeTestCondStrategy): cs.Result = {
        import cs._

        def default =
          // do type test first to ensure we won't select outer on null
          if (outerTestNeeded) and(typeTest(testedBinder, expectedTp), outerTest(testedBinder, expectedTp))
          else typeTest(testedBinder, expectedTp)

        // propagate expected type
        @inline def expTp(t: Tree): t.type = t setType expectedTp

        // true when called to type-test the argument to an extractor
        // don't do any fancy equality checking, just test the type
        if (extractorArgTypeTest) default
        else expectedTp match {
          // TODO: [SPEC] the spec requires `eq` instead of `==` for singleton types
          // this implies sym.isStable
          case SingleType(_, sym)                       => and(equalsTest(CODE.REF(sym), testedBinder), typeTest(testedBinder, expectedTp.widen))
          // must use == to support e.g. List() == Nil
          case ThisType(sym) if sym.isModule            => and(equalsTest(CODE.REF(sym), testedBinder), typeTest(testedBinder, expectedTp.widen))
          case ConstantType(Constant(null)) if testedBinder.info.widen <:< AnyRefClass.tpe
                                                        => eqTest(expTp(CODE.NULL), testedBinder)
          case ConstantType(const)                      => equalsTest(expTp(Literal(const)), testedBinder)
          case ThisType(sym)                            => eqTest(expTp(This(sym)), testedBinder)

          // TODO: verify that we don't need to special-case Array
          // I think it's okay:
          //  - the isInstanceOf test includes a test for the element type
          //  - Scala's arrays are invariant (so we don't drop type tests unsoundly)
          case _ if (expectedTp <:< AnyRefClass.tpe) && !needsTypeTest(testedBinder.info.widen, expectedTp) =>
            // do non-null check first to ensure we won't select outer on null
            if (outerTestNeeded) and(nonNullTest(testedBinder), outerTest(testedBinder, expectedTp))
            else nonNullTest(testedBinder)

          case _ => default
        }
      }

      val cond = renderCondition(treeCondStrategy)
      val res  = codegen._asInstanceOf(testedBinder, nextBinderTp)

      // is this purely a type test, e.g. no outer check, no equality tests (used in switch emission)
      def isPureTypeTest = renderCondition(pureTypeTestChecker)

      override def toString = "TT"+(expectedTp, testedBinder.name, nextBinderTp)
    }

    // need to substitute to deal with existential types -- TODO: deal with existentials better, don't substitute (see RichClass during quick.comp)
    case class EqualityTestTreeMaker(prevBinder: Symbol, patTree: Tree, override val pos: Position) extends CondTreeMaker {
      val nextBinderTp = prevBinder.info.widen

      // NOTE: generate `patTree == patBinder`, since the extractor must be in control of the equals method (also, patBinder may be null)
      // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where it's required)
      val cond = codegen._equals(patTree, prevBinder)
      val res  = CODE.REF(prevBinder)
      override def toString = "ET"+(prevBinder.name, patTree)
    }

    case class AlternativesTreeMaker(prevBinder: Symbol, var altss: List[List[TreeMaker]], pos: Position) extends TreeMaker with NoNewBinders {
      // don't substitute prevBinder to nextBinder, a set of alternatives does not need to introduce a new binder, simply reuse the previous one

      override private[TreeMakers] def incorporateOuterSubstitution(outerSubst: Substitution): Unit = {
        super.incorporateOuterSubstitution(outerSubst)
        altss = altss map (alts => propagateSubstitution(alts, substitution))
      }

      def chainBefore(next: Tree)(codegenAlt: Casegen): Tree = { import CODE._
        atPos(pos){
          // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
          // (for now,) alternatives may not bind variables (except wildcards), so we don't care about the final substitution built internally by makeTreeMakers
          val combinedAlts = altss map (altTreeMakers =>
            ((casegen: Casegen) => combineExtractors(altTreeMakers :+ TrivialTreeMaker(casegen.one(TRUE_typed)))(casegen))
          )

          val findAltMatcher = codegenAlt.matcher(EmptyTree, NoSymbol, BooleanClass.tpe)(combinedAlts, Some(x => FALSE_typed))
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
    // it drops SubstOnly tree makers, since their only goal in life is to propagate substitutions to the next tree maker, which is fullfilled by propagateSubstitution
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

    def combineCasesNoSubstOnly(scrut: Tree, scrutSym: Symbol, casesNoSubstOnly: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree =
      fixerUpper(owner, scrut.pos){
        val ptDefined    = if (isFullyDefined(pt)) pt else NoType
        def matchFailGen = (matchFailGenOverride orElse Some(CODE.MATCHERROR(_: Tree)))
        // patmatDebug("combining cases: "+ (casesNoSubstOnly.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        def isSwitchAnnotation(tpe: Type) = tpe hasAnnotation SwitchClass
        def isUncheckedAnnotation(tpe: Type) = tpe hasAnnotation UncheckedClass

        val (unchecked, requireSwitch) =
          if (settings.XnoPatmatAnalysis.value) (true, false)
          else scrut match {
            case Typed(_, tpt) =>
              (isUncheckedAnnotation(tpt.tpe),
               // matches with two or fewer cases need not apply for switchiness (if-then-else will do)
               isSwitchAnnotation(tpt.tpe) && casesNoSubstOnly.lengthCompare(2) > 0)
            case _ =>
              (false, false)
          }

        emitSwitch(scrut, scrutSym, casesNoSubstOnly, pt, matchFailGenOverride).getOrElse{
          if (requireSwitch) typer.context.unit.warning(scrut.pos, "could not emit switch for @switch annotated match")

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

            val (cases, toHoist) = optimizeCases(scrutSym, casesNoSubstOnly, pt, unchecked)

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
            // patmatDebug("new symbol for "+ (t, t.symbol.ownerChain))
          case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == origOwner) =>
            // patmatDebug("fundef: "+ (t, t.symbol.ownerChain, currentOwner.ownerChain))
            t.symbol.owner = currentOwner
          case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == origOwner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
            // patmatDebug("def: "+ (d, d.symbol.ownerChain, currentOwner.ownerChain))
            if(d.symbol.isLazy) { // for lazy val's accessor -- is there no tree??
              assert(d.symbol.lazyAccessor != NoSymbol && d.symbol.lazyAccessor.owner == d.symbol.owner, d.symbol.lazyAccessor)
              d.symbol.lazyAccessor.owner = currentOwner
            }
            if(d.symbol.moduleClass ne NoSymbol)
              d.symbol.moduleClass.owner = currentOwner

            d.symbol.owner = currentOwner
          // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
          // patmatDebug("untouched "+ (t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain))
          case _ =>
        }
        super.traverse(t)
      }

      // override def apply
      // patmatDebug("before fixerupper: "+ xTree)
      // currentRun.trackerFactory.snapshot()
      // patmatDebug("after fixerupper")
      // currentRun.trackerFactory.snapshot()
    }
  }


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// generate actual trees
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait CodegenCore extends MatchMonadInterface {
    private var ctr = 0
    def freshName(prefix: String) = {ctr += 1; vpmName.counted(prefix, ctr)}

    // assert(owner ne null); assert(owner ne NoSymbol)
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x") =
      NoSymbol.newTermSymbol(freshName(prefix), pos) setInfo tp

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree

      // local / context-free
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      def _equals(checker: Tree, binder: Symbol): Tree
      def _isInstanceOf(b: Symbol, tp: Type): Tree
      def and(a: Tree, b: Tree): Tree
      def drop(tgt: Tree)(n: Int): Tree
      def index(tgt: Tree)(i: Int): Tree
      def mkZero(tp: Type): Tree
      def tupleSel(binder: Symbol)(i: Int): Tree
    }

    // structure
    trait Casegen extends AbsCodegen { import CODE._
      def one(res: Tree): Tree

      def flatMap(prev: Tree, b: Symbol, next: Tree): Tree
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree
      def flatMapGuard(cond: Tree, next: Tree): Tree
      def ifThenElseZero(c: Tree, then: Tree): Tree = IF (c) THEN then ELSE zero
      protected def zero: Tree
    }

    def codegen: AbsCodegen

    def typesConform(tp: Type, pt: Type) = ((tp eq pt) || (tp <:< pt))

    abstract class CommonCodegen extends AbsCodegen { import CODE._
      def fun(arg: Symbol, body: Tree): Tree           = Function(List(ValDef(arg)), body)
      def genTypeApply(tfun: Tree, args: Type*): Tree  = if(args contains NoType) tfun else TypeApply(tfun, args.toList map TypeTree)
      def tupleSel(binder: Symbol)(i: Int): Tree       = (REF(binder) DOT nme.productAccessorName(i)) // make tree that accesses the i'th component of the tuple referenced by binder
      def index(tgt: Tree)(i: Int): Tree               = tgt APPLY (LIT(i))
      def drop(tgt: Tree)(n: Int): Tree                = (tgt DOT vpmName.drop) (LIT(n))
      def _equals(checker: Tree, binder: Symbol): Tree = checker MEMBER_== REF(binder)          // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def and(a: Tree, b: Tree): Tree                  = a AND b

      // drop annotations generated by CPS plugin etc, since its annotationchecker rejects T @cps[U] <: Any
      // let's assume for now annotations don't affect casts, drop them there, and bring them back using the outer Typed tree
      private def mkCast(t: Tree, tp: Type) =
        Typed(gen.mkAsInstanceOf(t, tp.withoutAnnotations, true, false), TypeTree() setType tp)

      // the force is needed mainly to deal with the GADT typing hack (we can't detect it otherwise as tp nor pt need contain an abstract type, we're just casting wildly)
      def _asInstanceOf(t: Tree, tp: Type, force: Boolean = false): Tree = if (!force && (t.tpe ne NoType) && t.isTyped && typesConform(t.tpe, tp)) t else mkCast(t, tp)
      def _asInstanceOf(b: Symbol, tp: Type): Tree = if (typesConform(b.info, tp)) REF(b) else mkCast(REF(b), tp)
      def _isInstanceOf(b: Symbol, tp: Type): Tree = gen.mkIsInstanceOf(REF(b), tp.withoutAnnotations, true, false)
      //   if (typesConform(b.info, tpX)) { patmatDebug("warning: emitted spurious isInstanceOf: "+(b, tp)); TRUE }

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
  }

  trait PureMatchMonadInterface extends MatchMonadInterface {
    val matchStrategy: Tree

    def inMatchMonad(tp: Type): Type = appliedType(oneSig, List(tp)).finalResultType
    def pureType(tp: Type): Type     = appliedType(oneSig, List(tp)).paramTypes.headOption getOrElse NoType // fail gracefully (otherwise we get crashes)
    protected def matchMonadSym      = oneSig.finalResultType.typeSymbol

    import CODE._
    def _match(n: Name): SelectStart = matchStrategy DOT n

    private lazy val oneSig: Type =
      typer.typed(_match(vpmName.one), EXPRmode | POLYmode | TAPPmode | FUNmode, WildcardType).tpe  // TODO: error message
  }

  trait PureCodegen extends CodegenCore with PureMatchMonadInterface {
    def codegen: AbsCodegen = pureCodegen

    object pureCodegen extends CommonCodegen with Casegen { import CODE._
      //// methods in MatchingStrategy (the monad companion) -- used directly in translation
      // __match.runOrElse(`scrut`)(`scrutSym` => `matcher`)
      // TODO: consider catchAll, or virtualized matching will break in exception handlers
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree =
        _match(vpmName.runOrElse) APPLY (scrut) APPLY (fun(scrutSym, cases map (f => f(this)) reduceLeft typedOrElse))

      // __match.one(`res`)
      def one(res: Tree): Tree = (_match(vpmName.one)) (res)
      // __match.zero
      protected def zero: Tree = _match(vpmName.zero)
      // __match.guard(`c`, `then`)
      def guard(c: Tree, then: Tree): Tree = _match(vpmName.guard) APPLY (c, then)

      //// methods in the monad instance -- used directly in translation
      // `prev`.flatMap(`b` => `next`)
      def flatMap(prev: Tree, b: Symbol, next: Tree): Tree = (prev DOT vpmName.flatMap)(fun(b, next))
      // `thisCase`.orElse(`elseCase`)
      def typedOrElse(thisCase: Tree, elseCase: Tree): Tree = (thisCase DOT vpmName.orElse) APPLY (elseCase)
      //  __match.guard(`cond`, `res`).flatMap(`nextBinder` => `next`)
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree = flatMap(guard(cond, res), nextBinder, next)
      //  __match.guard(`guardTree`, ()).flatMap((_: P[Unit]) => `next`)
      def flatMapGuard(guardTree: Tree, next: Tree): Tree = flatMapCond(guardTree, CODE.UNIT, freshSym(guardTree.pos, pureType(UnitClass.tpe)), next)
    }
  }


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// OPTIMIZATIONS
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// decisions, decisions
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait TreeMakerApproximation extends TreeMakers with Prettification{ self: CodegenCore =>
    object Test {
      var currId = 0
    }
    case class Test(cond: Cond, treeMaker: TreeMaker) {
      // def <:<(other: Test) = cond <:< other.cond
      // def andThen_: (prev: List[Test]): List[Test] =
      //   prev.filterNot(this <:< _) :+ this

      // private val reusedBy = new collection.mutable.HashSet[Test]
      var reuses: Option[Test] = None
      def registerReuseBy(later: Test): Unit = {
        assert(later.reuses.isEmpty, later.reuses)
        // reusedBy += later
        later.reuses = Some(this)
      }

      val id = { Test.currId += 1; Test.currId}
      override def toString =
        "T"+ id + "C("+ cond +")"  //+ (reuses map ("== T"+_.id) getOrElse (if(reusedBy.isEmpty) treeMaker else reusedBy mkString (treeMaker+ " -->(", ", ",")")))
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
    case object Top extends Cond {override def toString = "T"}


    // takes away knowledge. e.g., a user-defined guard
    case object Havoc extends Cond {override def toString = "_|_"}

    // we know everything! everything!
    // this either means the case is unreachable,
    // or that it is statically known to be picked -- at this point in the decision tree --> no point in emitting further alternatives
    // case object Bottom extends Cond


    case class AndCond(a: Cond, b: Cond) extends Cond {override def toString = a +"/\\"+ b}
    case class OrCond(a: Cond, b: Cond) extends Cond  {override def toString = "("+a+") \\/ ("+ b +")"}

    object EqualityCond {
      private val uniques = new collection.mutable.HashMap[(Tree, Tree), EqualityCond]
      def apply(testedPath: Tree, rhs: Tree): EqualityCond = uniques getOrElseUpdate((testedPath, rhs), new EqualityCond(testedPath, rhs))
      def unapply(c: EqualityCond) = Some(c.testedPath, c.rhs)
    }
    class EqualityCond(val testedPath: Tree, val rhs: Tree) extends Cond {
      // def negation         = TopCond // inequality doesn't teach us anything
      // do simplification when we know enough about the tree statically:
      //  - collapse equal trees
      //  - accumulate tests when (in)equality not known statically
      //  - become bottom when we statically know this can never match

      override def toString = testedPath +" == "+ rhs +"#"+ id
    }

    object NonNullCond {
      private val uniques = new collection.mutable.HashMap[Tree, NonNullCond]
      def apply(testedPath: Tree): NonNullCond = uniques getOrElseUpdate(testedPath, new NonNullCond(testedPath))
      def unapply(c: NonNullCond) = Some(c.testedPath)
    }
    class NonNullCond(val testedPath: Tree) extends Cond {
      override def toString = testedPath +" ne null " +"#"+ id
    }

    object TypeCond {
      private val uniques = new collection.mutable.HashMap[(Tree, Type), TypeCond]
      def apply(testedPath: Tree, pt: Type): TypeCond = uniques getOrElseUpdate((testedPath, pt), new TypeCond(testedPath, pt))
      def unapply(c: TypeCond) = Some(c.testedPath, c.pt)
    }
    class TypeCond(val testedPath: Tree, val pt: Type) extends Cond {
      // def negation         = TopCond // inequality doesn't teach us anything
      // do simplification when we know enough about the tree statically:
      //  - collapse equal trees
      //  - accumulate tests when (in)equality not known statically
      //  - become bottom when we statically know this can never match
      override def toString = testedPath +" : "+ pt +"#"+ id
    }

//    class OuterEqCond(val testedPath: Tree, val expectedType: Type) extends Cond {
//      val expectedOuter = expectedTp.prefix match {
//        case ThisType(clazz)  => THIS(clazz)
//        case pre              => REF(pre.prefix, pre.termSymbol)
//      }
//
//      // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
//      // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
//      val outer = expectedTp.typeSymbol.newMethod(vpmName.outer) setInfo expectedTp.prefix setFlag SYNTHETIC
//
//      (Select(codegen._asInstanceOf(testedBinder, expectedTp), outer)) OBJ_EQ expectedOuter
//    }


    // returns (tree, tests), where `tree` will be used to refer to `root` in `tests`
    abstract class TreeMakersToConds(val root: Symbol) {
      def discard() = {
        pointsToBound.clear()
        trees.clear()
        normalize  = EmptySubstitution
        accumSubst = EmptySubstitution
      }
      // a variable in this set should never be replaced by a tree that "does not consist of a selection on a variable in this set" (intuitively)
      private val pointsToBound = collection.mutable.HashSet(root)
      private val trees         = collection.mutable.HashSet.empty[Tree]

      // the substitution that renames variables to variables in pointsToBound
      private var normalize: Substitution  = EmptySubstitution

      // replaces a variable (in pointsToBound) by a selection on another variable in pointsToBound
      // in the end, instead of having x1, x1.hd, x2, x2.hd, ... flying around,
      // we want something like x1, x1.hd, x1.hd.tl, x1.hd.tl.hd, so that we can easily recognize when
      // we're testing the same variable
      // TODO check:
      //   pointsToBound -- accumSubst.from == Set(root) && (accumSubst.from.toSet -- pointsToBound) isEmpty
      private var accumSubst: Substitution = EmptySubstitution

      private def updateSubstitution(subst: Substitution) = {
        // find part of substitution that replaces bound symbols by new symbols, and reverse that part
        // so that we don't introduce new aliases for existing symbols, thus keeping the set of bound symbols minimal
        val (boundSubst, unboundSubst) = (subst.from zip subst.to) partition {
          case (f, t) =>
            t.isInstanceOf[Ident] && (t.symbol ne NoSymbol) && pointsToBound(f)
        }
        val (boundFrom, boundTo) = boundSubst.unzip
        val (unboundFrom, unboundTo) = unboundSubst.unzip

        // reverse substitution that would otherwise replace a variable we already encountered by a new variable
        // NOTE: this forgets the more precise type we have for these later variables, but that's probably okay
        normalize >>= Substitution(boundTo map (_.symbol), boundFrom map (CODE.REF(_)))
        // patmatDebug("normalize: "+ normalize)

        val okSubst = Substitution(unboundFrom, unboundTo map (normalize(_))) // it's important substitution does not duplicate trees here -- it helps to keep hash consing simple, anyway
        pointsToBound ++= ((okSubst.from, okSubst.to).zipped filter { (f, t) => pointsToBound exists (sym => t.exists(_.symbol == sym)) })._1
        // patmatDebug("pointsToBound: "+ pointsToBound)

        accumSubst >>= okSubst
        // patmatDebug("accumSubst: "+ accumSubst)
      }


      // TODO: improve, e.g., for constants
      def sameValue(a: Tree, b: Tree): Boolean = (a eq b) || ((a, b) match {
        case (_ : Ident, _ : Ident) => a.symbol eq b.symbol
        case _                      => false
      })

      // hashconsing trees (modulo value-equality)
      def unique(t: Tree, tpOverride: Type = NoType): Tree =
        trees find (a => a.equalsStructure0(t)(sameValue)) match {
          case Some(orig) => orig // patmatDebug("unique: "+ (t eq orig, orig));
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
        unique(accumSubst(normalize(CODE.REF(b))), b.tpe)

      @inline def /\(conds: Iterable[Cond]) = if (conds.isEmpty) Top else conds.reduceLeft(AndCond(_, _))
      @inline def \/(conds: Iterable[Cond]) = if (conds.isEmpty) Havoc else conds.reduceLeft(OrCond(_, _))

      // note that the sequencing of operations is important: must visit in same order as match execution
      // binderToUniqueTree uses the type of the first symbol that was encountered as the type for all future binders
      final protected def treeMakerToCond(tm: TreeMaker, condMaker: CondMaker): Cond = {
        updateSubstitution(tm.substitution)
        condMaker(tm)(treeMakerToCond(_, condMaker))
      }

      final protected def treeMakerToCondNoSubst(tm: TreeMaker, condMaker: CondMaker): Cond =
        condMaker(tm)(treeMakerToCondNoSubst(_, condMaker))

      type CondMaker = TreeMaker => (TreeMaker => Cond) => Cond
      final def makeCond(tm: TreeMaker)(recurse: TreeMaker => Cond): Cond = {
        tm match {
          case ttm@TypeTestTreeMaker(prevBinder, testedBinder, pt, _)   =>
            object condStrategy extends TypeTestTreeMaker.TypeTestCondStrategy {
              type Result                                           = Cond
              def and(a: Result, b: Result)                         = AndCond(a, b)
              def outerTest(testedBinder: Symbol, expectedTp: Type) = Top // TODO OuterEqCond(testedBinder, expectedType)
              def typeTest(b: Symbol, pt: Type) = { // a type test implies the tested path is non-null (null.isInstanceOf[T] is false for all T)
                val p = binderToUniqueTree(b);                        AndCond(NonNullCond(p), TypeCond(p, uniqueTp(pt)))
              }
              def nonNullTest(testedBinder: Symbol)                 = NonNullCond(binderToUniqueTree(testedBinder))
              def equalsTest(pat: Tree, testedBinder: Symbol)       = EqualityCond(binderToUniqueTree(testedBinder), unique(pat))
              def eqTest(pat: Tree, testedBinder: Symbol)           = EqualityCond(binderToUniqueTree(testedBinder), unique(pat)) // TODO: eq, not ==
            }
            ttm.renderCondition(condStrategy)
          case EqualityTestTreeMaker(prevBinder, patTree, _)        => EqualityCond(binderToUniqueTree(prevBinder), unique(patTree))
          case AlternativesTreeMaker(_, altss, _)                   => \/(altss map (alts => /\(alts map recurse)))
          case ProductExtractorTreeMaker(testedBinder, None, subst) => NonNullCond(binderToUniqueTree(testedBinder))
          case ExtractorTreeMaker(_, _, _, _)
             | GuardTreeMaker(_)
             | ProductExtractorTreeMaker(_, Some(_), _)
             | BodyTreeMaker(_, _)                                  => Havoc
          case SubstOnlyTreeMaker(_, _)                             => Top
        }
      }

      final def approximateMatch(cases: List[List[TreeMaker]], condMaker: CondMaker = makeCond): List[List[Test]] = cases.map { _ map (tm => Test(treeMakerToCond(tm, condMaker), tm)) }

      final def approximateMatchAgain(cases: List[List[TreeMaker]], condMaker: CondMaker = makeCond): List[List[Test]] = cases.map { _ map (tm => Test(treeMakerToCondNoSubst(tm, condMaker), tm)) }
    }

    def approximateMatch(root: Symbol, cases: List[List[TreeMaker]]): List[List[Test]] = {
      object approximator extends TreeMakersToConds(root)
      approximator.approximateMatch(cases)
    }

    def showTreeMakers(cases: List[List[TreeMaker]]) = {
      patmatDebug("treeMakers:")
      patmatDebug(alignAcrossRows(cases, ">>"))
    }

    def showTests(testss: List[List[Test]]) = {
      patmatDebug("tests: ")
      patmatDebug(alignAcrossRows(testss, "&"))
    }
  }

  trait Prettification {
    private def max(xs: Seq[Int]) = if (xs isEmpty) 0 else xs max

    def alignedColumns(cols: Seq[AnyRef]): Seq[String] = {
      def toString(x: AnyRef) = if (x eq null) "" else x.toString
      if (cols.isEmpty || cols.tails.isEmpty) cols map toString
      else {
        val (colStrs, colLens) = cols map {c => val s = toString(c); (s, s.length)} unzip
        val maxLen = max(colLens)
        val avgLen = colLens.sum/colLens.length
        val goalLen = maxLen min avgLen*2
        def pad(s: String) = {
          val toAdd = ((goalLen - s.length) max 0) + 2
          (" " * (toAdd/2)) + s + (" " * (toAdd/2 + (toAdd%2)))
        }
        cols map (x => pad(toString(x)))
      }
    }
    def alignAcrossRows(xss: List[List[AnyRef]], sep: String, lineSep: String = "\n"): String = {
      val maxLen = max(xss map (_.length))
      val padded = xss map (xs => xs ++ List.fill(maxLen - xs.length)(null))
      padded.transpose.map(alignedColumns).transpose map (_.mkString(sep)) mkString(lineSep)
    }
  }

  // http://www.cis.upenn.edu/~cis510/tcl/chap3.pdf
  // http://users.encs.concordia.ca/~ta_ahmed/ms_thesis.pdf
  trait Logic extends Prettification {
    class Prop
    case class Eq(p: Var, q: Const) extends Prop

    type Const <: AbsConst

    trait AbsConst {
      def implies(other: Const): Boolean
      def excludes(other: Const): Boolean
    }

    type Var <: AbsVar

    trait AbsVar {
      // indicate we may later require a prop for V = C
      def registerEquality(c: Const): Unit

      // indicates null is part of the domain
      def considerNull: Unit

      // compute the domain and return it (call considerNull first!)
      def domainSyms: Option[Set[Sym]]

      // for this var, call it V, turn V = C into the equivalent proposition in boolean logic
      // registerEquality(c) must have been called prior to this call
      // in fact, all equalities relevant to this variable must have been registered
      def propForEqualsTo(c: Const): Prop

      // populated by registerEquality
      // once equalitySyms has been called, must not call registerEquality anymore
      def equalitySyms: List[Sym]
    }

    // would be nice to statically check whether a prop is equational or pure,
    // but that requires typing relations like And(x: Tx, y: Ty) : (if(Tx == PureProp && Ty == PureProp) PureProp else Prop)
    case class And(a: Prop, b: Prop) extends Prop
    case class Or(a: Prop, b: Prop) extends Prop
    case class Not(a: Prop) extends Prop

    case object True extends Prop
    case object False extends Prop

    // symbols are propositions
    case class Sym(val variable: Var, val const: Const) extends Prop {
      private[this] val id = nextSymId
      override def toString = variable +"="+ const +"#"+ id
    }
    private def nextSymId = {_symId += 1; _symId}; private var _symId = 0


    @inline def /\(props: Iterable[Prop]) = if (props.isEmpty) True else props.reduceLeft(And(_, _))
    @inline def \/(props: Iterable[Prop]) = if (props.isEmpty) False else props.reduceLeft(Or(_, _))


    trait PropTraverser {
      def apply(x: Prop): Unit = x match {
        case And(a, b) => apply(a); apply(b)
        case Or(a, b) => apply(a); apply(b)
        case Not(a) => apply(a)
        case Eq(a, b) => applyVar(a); applyConst(b)
        case _ =>
      }
      def applyVar(x: Var): Unit = {}
      def applyConst(x: Const): Unit = {}
    }

    def gatherVariables(p: Prop): Set[Var] = {
      val vars = new HashSet[Var]()
      (new PropTraverser {
        override def applyVar(v: Var) = vars += v
      })(p)
      vars.toSet
    }

    trait PropMap {
      def apply(x: Prop): Prop = x match { // TODO: mapConserve
        case And(a, b) => And(apply(a), apply(b))
        case Or(a, b) => Or(apply(a), apply(b))
        case Not(a) => Not(apply(a))
        case p => p
      }
    }

    // convert finite domain propositional logic with subtyping to pure boolean propositional logic
    // a type test or a value equality test are modelled as a variable being equal to some constant
    // a variable V may be assigned multiple constants, as long as they do not contradict each other
    // according to subtyping, e.g., V = ConstantType(1) and V = Int are valid assignments
    // we rewrite V = C to a fresh boolean symbol, and model what we know about the variable's domain
    // in a prelude (the equality axioms)
    //   1. a variable with a closed domain (of a sealed type) must be assigned one of the instantiatable types in its domain
    //   2. for each variable V in props, and each constant C it is compared to,
    //      compute which assignments imply each other (as in the example above: V = 1 implies V = Int)
    //      and which assignments are mutually exclusive (V = String implies -(V = Int))
    //
    // note that this is a conservative approximation: V = Constant(A) and V = Constant(B)
    // are considered mutually exclusive (and thus both cases are considered reachable in {case A => case B =>}),
    // even though A may be equal to B   (and thus the second case is not "dynamically reachable")
    //
    // TODO: for V1 representing x1 and V2 standing for x1.head, encode that
    //       V1 = Nil implies -(V2 = Ci) for all Ci in V2's domain (i.e., it is unassignable)
    def removeVarEq(props: List[Prop], considerNull: Boolean = false): (Prop, List[Prop]) = {
      val start = startTimer(patmatAnaVarEq)

      val vars = new collection.mutable.HashSet[Var]

      object gatherEqualities extends PropTraverser {
        override def apply(p: Prop) = p match {
          case Eq(v, c) =>
            vars += v
            v.registerEquality(c)
          case _ => super.apply(p)
        }
      }

      object rewriteEqualsToProp extends PropMap {
        override def apply(p: Prop) = p match {
          case Eq(v, c) => v.propForEqualsTo(c)
          case _ => super.apply(p)
        }
      }

      props foreach gatherEqualities.apply
      if (considerNull) vars foreach (_.considerNull)

      val pure = props map rewriteEqualsToProp.apply

      var eqAxioms: Prop = True
      @inline def addAxiom(p: Prop) = eqAxioms = And(eqAxioms, p)

      case class ExcludedPair(a: Const, b: Const) {
        override def equals(o: Any) = o match {
          case ExcludedPair(aa, bb) => (a == aa && b == bb) || (a == bb && b == aa)
          case _ => false
        }
        // make ExcludedPair(a, b).hashCode == ExcludedPair(b, a).hashCode
        override def hashCode = a.hashCode ^ b.hashCode
      }

      // patmatDebug("vars: "+ vars)
      vars.foreach { v =>
        val excludedPair = new collection.mutable.HashSet[ExcludedPair]

        // if v.domainSyms.isEmpty, we must consider the domain to be infinite
        // otherwise, since the domain fully partitions the type of the value,
        // exactly one of the types (and whatever it implies, imposed separately) must be chosen
        // consider X ::= A | B | C, and A => B
        // coverage is formulated as: A \/ B \/ C and the implications are
        v.domainSyms foreach { dsyms => addAxiom(\/(dsyms)) }

        val syms = v.equalitySyms
        // patmatDebug("eqSyms "+(v, syms))
        syms foreach { sym =>
          // if we've already excluded the pair at some point (-A \/ -B), then don't exclude the symmetric one (-B \/ -A)
          // (nor the positive implications -B \/ A, or -A \/ B, which would entail the equality axioms falsifying the whole formula)
          val todo = syms filterNot (b => (b.const == sym.const) || excludedPair(ExcludedPair(b.const, sym.const)))
          val (excluded, notExcluded) = todo partition (b => sym.const.excludes(b.const))
          val implied = notExcluded filter (b => sym.const.implies(b.const))
          // patmatDebug("implications: "+ (sym.const, excluded, implied, syms))

          // when this symbol is true, what must hold...
          implied  foreach (impliedSym => addAxiom(Or(Not(sym), impliedSym)))

          // ... and what must not?
          excluded foreach {excludedSym =>
            excludedPair += ExcludedPair(sym.const, excludedSym.const)
            addAxiom(Or(Not(sym), Not(excludedSym)))
          }
        }
      }

      // patmatDebug("eqAxioms:\n"+ cnfString(eqFreePropToSolvable(eqAxioms)))
      // patmatDebug("pure:\n"+ cnfString(eqFreePropToSolvable(pure)))

      stopTimer(patmatAnaVarEq, start)

      (eqAxioms, pure)
    }


    type Formula
    def andFormula(a: Formula, b: Formula): Formula

    class CNFBudgetExceeded extends RuntimeException("CNF budget exceeded")

    // may throw an CNFBudgetExceeded
    def propToSolvable(p: Prop) = {
      val (eqAxioms, pure :: Nil) = removeVarEq(List(p), considerNull = false)
      eqFreePropToSolvable(And(eqAxioms, pure))
    }

    def eqFreePropToSolvable(p: Prop): Formula
    def cnfString(f: Formula): String

    type Model = Map[Sym, Boolean]
    val EmptyModel: Model
    val NoModel: Model

    def findModelFor(f: Formula): Model
    def findAllModelsFor(f: Formula): List[Model]
  }

  trait CNF extends Logic {
    // CNF: a formula is a conjunction of clauses
    type Formula = Array[Clause]
    def formula(c: Clause*): Formula = c.toArray
    def andFormula(a: Formula, b: Formula): Formula = a ++ b

    // a clause is a disjunction of distinct literals
    type Clause = Set[Lit]
    def clause(l: Lit*): Clause = l.toSet
    @inline private def merge(a: Clause, b: Clause) = a ++ b

    type Lit
    def Lit(sym: Sym, pos: Boolean = true): Lit

    // throws an CNFBudgetExceeded when the prop results in a CNF that's too big
    def eqFreePropToSolvable(p: Prop): Formula = {
      // TODO: for now, reusing the normalization from DPLL
      def negationNormalForm(p: Prop): Prop = p match {
        case And(a, b)      => And(negationNormalForm(a), negationNormalForm(b))
        case Or(a, b)       => Or(negationNormalForm(a), negationNormalForm(b))
        case Not(And(a, b)) => negationNormalForm(Or(Not(a), Not(b)))
        case Not(Or(a, b))  => negationNormalForm(And(Not(a), Not(b)))
        case Not(Not(p))    => negationNormalForm(p)
        case Not(True)      => False
        case Not(False)     => True
        case True
           | False
           | (_ : Sym)
           | Not(_ : Sym)   => p
      }

      val TrueF          = formula()
      val FalseF         = formula(clause())
      def lit(s: Sym)    = formula(clause(Lit(s)))
      def negLit(s: Sym) = formula(clause(Lit(s, false)))

      def conjunctiveNormalForm(p: Prop, budget: Int = 256): Formula = {
        def distribute(a: Formula, b: Formula, budget: Int): Formula =
          if (budget <= 0) throw new CNFBudgetExceeded
          else
            (a, b) match {
              // true \/ _ = true
              // _ \/ true = true
              case (trueA, trueB) if trueA.size == 0 || trueB.size == 0 => TrueF
              // lit \/ lit
              case (a, b) if a.size == 1 && b.size == 1 => formula(merge(a(0), b(0)))
              // (c1 /\ ... /\ cn) \/ d = ((c1 \/ d) /\ ... /\ (cn \/ d))
              // d \/ (c1 /\ ... /\ cn) = ((d \/ c1) /\ ... /\ (d \/ cn))
              case (cs, ds) =>
                val (big, small) = if (cs.size > ds.size) (cs, ds) else (ds, cs)
                big flatMap (c => distribute(formula(c), small, budget - (big.size*small.size)))
            }

        if (budget <= 0) throw new CNFBudgetExceeded

        p match {
          case True        => TrueF
          case False       => FalseF
          case s: Sym      => lit(s)
          case Not(s: Sym) => negLit(s)
          case And(a, b)   =>
            val cnfA = conjunctiveNormalForm(a, budget - 1)
            val cnfB = conjunctiveNormalForm(b, budget - cnfA.size)
            cnfA ++ cnfB
          case Or(a, b)    =>
            val cnfA = conjunctiveNormalForm(a)
            val cnfB = conjunctiveNormalForm(b)
            distribute(cnfA, cnfB, budget - (cnfA.size + cnfB.size))
        }
      }

      val start = startTimer(patmatCNF)
      val res = conjunctiveNormalForm(negationNormalForm(p))
      stopTimer(patmatCNF, start)
      patmatCNFSizes(res.size) += 1

//      patmatDebug("cnf for\n"+ p +"\nis:\n"+cnfString(res))
      res
    }

  }

  trait DPLLSolver extends CNF {
    // a literal is a (possibly negated) variable
    def Lit(sym: Sym, pos: Boolean = true) = new Lit(sym, pos)
    class Lit(val sym: Sym, val pos: Boolean) {
      override def toString = if (!pos) "-"+ sym.toString else sym.toString
      override def equals(o: Any) = o match {
        case o: Lit => (o.sym == sym) && (o.pos == pos)
        case _ => false
      }
      override def hashCode = sym.hashCode + pos.hashCode

      def unary_- = Lit(sym, !pos)
    }

    def cnfString(f: Formula) = alignAcrossRows(f map (_.toList) toList, "\\/", " /\\\n")

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)
    val EmptyModel = Map.empty[Sym, Boolean]
    val NoModel: Model = null

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(f: Formula): List[Model] = {
      val vars: Set[Sym] = f.flatMap(_ collect {case l: Lit => l.sym}).toSet
      // patmatDebug("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      def negateModel(m: Model) = clause(m.toSeq.map{ case (sym, pos) => Lit(sym, !pos) } : _*)

      def findAllModels(f: Formula, models: List[Model], recursionDepthAllowed: Int = 10): List[Model]=
        if (recursionDepthAllowed == 0) models
        else {
          // patmatDebug("solving\n"+ cnfString(f))
          val model = findModelFor(f)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoModel) {
            val unassigned = (vars -- model.keySet).toList
            // patmatDebug("unassigned "+ unassigned +" in "+ model)
            def force(lit: Lit) = {
              val model = withLit(findModelFor(dropUnit(f, lit)), lit)
              if (model ne NoModel) List(model)
              else Nil
            }
            val forced = unassigned flatMap { s =>
              force(Lit(s, true)) ++ force(Lit(s, false))
            }
            // patmatDebug("forced "+ forced)
            val negated = negateModel(model)
            findAllModels(f :+ negated, model :: (forced ++ models), recursionDepthAllowed - 1)
          }
          else models
        }

      findAllModels(f, Nil)
    }

    @inline private def withLit(res: Model, l: Lit): Model = if (res eq NoModel) NoModel else res + (l.sym -> l.pos)
    @inline private def dropUnit(f: Formula, unitLit: Lit) = {
      val negated = -unitLit
      // drop entire clauses that are trivially true
      // (i.e., disjunctions that contain the literal we're making true in the returned model),
      // and simplify clauses by dropping the negation of the literal we're making true
      // (since False \/ X == X)
      f.filterNot(_.contains(unitLit)).map(_ - negated)
    }

    def findModelFor(f: Formula): Model = {
      @inline def orElse(a: Model, b: => Model) = if (a ne NoModel) a else b

      // patmatDebug("dpll\n"+ cnfString(f))

      val start = startTimer(patmatAnaDPLL)

      val satisfiableWithModel: Model =
        if (f isEmpty) EmptyModel
        else if(f exists (_.isEmpty)) NoModel
        else f.find(_.size == 1) match {
          case Some(unitClause) =>
            val unitLit = unitClause.head
            // patmatDebug("unit: "+ unitLit)
            withLit(findModelFor(dropUnit(f, unitLit)), unitLit)
          case _ =>
            // partition symbols according to whether they appear in positive and/or negative literals
            val pos = new HashSet[Sym]()
            val neg = new HashSet[Sym]()
            f.foreach{_.foreach{ lit =>
              if (lit.pos) pos += lit.sym else neg += lit.sym
            }}
            // appearing in both positive and negative
            val impures = pos intersect neg
            // appearing only in either positive/negative positions
            val pures = (pos ++ neg) -- impures

            if (pures nonEmpty) {
              val pureSym = pures.head
              // turn it back into a literal
              // (since equality on literals is in terms of equality
              //  of the underlying symbol and its positivity, simply construct a new Lit)
              val pureLit = Lit(pureSym, pos(pureSym))
              // patmatDebug("pure: "+ pureLit +" pures: "+ pures +" impures: "+ impures)
              val simplified = f.filterNot(_.contains(pureLit))
              withLit(findModelFor(simplified), pureLit)
            } else {
              val split = f.head.head
              // patmatDebug("split: "+ split)
              orElse(findModelFor(f :+ clause(split)), findModelFor(f :+ clause(-split)))
            }
        }

        stopTimer(patmatAnaDPLL, start)

        satisfiableWithModel
    }
  }


  /**
   * Represent a match as a formula in propositional logic that encodes whether the match matches (abstractly: we only consider types)
   *
   */
  trait SymbolicMatchAnalysis extends TreeMakerApproximation with Logic { self: CodegenCore =>
    def prepareNewAnalysis() = { Var.resetUniques(); Const.resetUniques() }

    object Var {
      private var _nextId = 0
      def nextId = {_nextId += 1; _nextId}

      def resetUniques() = {_nextId = 0; uniques.clear()}
      private val uniques = new collection.mutable.HashMap[Tree, Var]
      def apply(x: Tree): Var = uniques getOrElseUpdate(x, new Var(x, x.tpe))
    }
    class Var(val path: Tree, fullTp: Type, checked: Boolean = true) extends AbsVar {
      private[this] val id: Int = Var.nextId

      // private[this] var canModify: Option[Array[StackTraceElement]] = None
      @inline private[this] def ensureCanModify = {} //if (canModify.nonEmpty) patmatDebug("BUG!"+ this +" modified after having been observed: "+ canModify.get.mkString("\n"))

      @inline private[this] def observed = {} //canModify = Some(Thread.currentThread.getStackTrace)

      // don't access until all potential equalities have been registered using registerEquality
      private[this] val symForEqualsTo = new collection.mutable.HashMap[Const, Sym]

      // when looking at the domain, we only care about types we can check at run time
      val domainTp: Type = checkableType(fullTp)

      private[this] var _considerNull = false
      def considerNull: Unit = { ensureCanModify; if (NullTp <:< domainTp) _considerNull = true }

      // case None => domain is unknown,
      // case Some(List(tps: _*)) => domain is exactly tps
      // we enumerate the subtypes of the full type, as that allows us to filter out more types statically,
      // once we go to run-time checks (on Const's), convert them to checkable types
      // TODO: there seems to be bug for singleton domains (variable does not show up in model)
      lazy val domain: Option[Set[Const]] =
        if (!checked) None
        else {
          val subConsts = enumerateSubtypes(fullTp).map{ tps =>
            tps.toSet[Type].map{ tp =>
              val domainC = TypeConst(tp)
              registerEquality(domainC)
              domainC
            }
          }

          val allConsts =
            if (! _considerNull) subConsts
            else {
              registerEquality(NullConst)
              subConsts map (_ + NullConst)
            }

          observed; allConsts
        }

      // accessing after calling considerNull will result in inconsistencies
      lazy val domainSyms: Option[Set[Sym]] = domain map { _ map symForEqualsTo }


      // populate equalitySyms
      // don't care about the result, but want only one fresh symbol per distinct constant c
      def registerEquality(c: Const): Unit = {ensureCanModify; symForEqualsTo getOrElseUpdate(c, Sym(this, c))}

      // don't access until all potential equalities have been registered using registerEquality
      lazy val equalitySyms = {observed; symForEqualsTo.values.toList}

      // return the symbol that represents this variable being equal to the constant `c`, if it exists, otherwise False (for robustness)
      // (registerEquality(c) must have been called prior, either when constructing the domain or from outside)
      def propForEqualsTo(c: Const): Prop = {observed; symForEqualsTo.getOrElse(c, False)}


      // don't call until all equalities have been registered and considerNull has been called (if needed)
      def describe = toString + ": " + fullTp + domain.map(_.mkString(" ::= ", " | ", "// "+ symForEqualsTo.keys)).getOrElse(symForEqualsTo.keys.mkString(" ::= ", " | ", " | ...")) + " // = " + path
      override def toString = "V"+ id
    }


    // all our variables range over types
    // a literal constant becomes ConstantType(Constant(v)) when the type allows it (roughly, anyval + string + null)
    // equality between variables: SingleType(x) (note that pattern variables cannot relate to each other -- it's always patternVar == nonPatternVar)
    object Const {
      def resetUniques() = {_nextTypeId = 0; _nextValueId = 0; uniques.clear()} // patmatDebug("RESET")

      private var _nextTypeId = 0
      def nextTypeId = {_nextTypeId += 1; _nextTypeId}

      private var _nextValueId = 0
      def nextValueId = {_nextValueId += 1; _nextValueId}

      private val uniques = new collection.mutable.HashMap[Type, Const]
      private[SymbolicMatchAnalysis] def unique(tp: Type, mkFresh: => Const): Const =
        uniques.get(tp).getOrElse(
          uniques.find {case (oldTp, oldC) => oldTp =:= tp} match {
            case Some((_, c)) => c
            case _ =>
              val fresh = mkFresh
              uniques(tp) = fresh
              fresh
          })
    }

    sealed abstract class Const extends AbsConst {
      protected def tp: Type
      protected def wideTp: Type

      def isAny = wideTp.typeSymbol == AnyClass

      final def implies(other: Const): Boolean = {
        val r = (this, other) match {
          case (_: ValueConst, _: ValueConst) => this == other // hashconsed
          case (_: ValueConst, _: TypeConst)  => tp <:< other.tp
          case (_: TypeConst,  _)             => tp <:< other.tp
          case _                              => false
        }
        // if(r) patmatDebug("implies    : "+(this, other))
        // else  patmatDebug("NOT implies: "+(this, other))
        r
      }

      // does V = C preclude V having value `other`?  V = null is an exclusive assignment,
      // but V = 1 does not preclude V = Int, or V = Any
      final def excludes(other: Const): Boolean = {
        val r = (this, other) match {
          case (_, NullConst)                 => true
          case (NullConst, _)                 => true
          // this causes false negative for unreachability, but that's ok:
          // example: val X = 1; val Y = 1; (2: Int) match { case X => case Y => /* considered reachable */ }
          case (_: ValueConst, _: ValueConst) => this != other
          case (_: ValueConst, _: TypeConst)  => !((tp <:< other.tp) || (other.tp <:< wideTp))
          case (_: TypeConst,  _: ValueConst) => !((other.tp <:< tp) || (tp <:< other.wideTp))
          case (_: TypeConst,  _: TypeConst)  => !((tp <:< other.tp) || (other.tp <:< tp))
          case _                              => false
        }
        // if(r) patmatDebug("excludes    : "+(this, other))
        // else  patmatDebug("NOT excludes: "+(this, other))
        r
      }

      // note: use reference equality on Const since they're hash-consed (doing type equality all the time is too expensive)
      // the equals inherited from AnyRef does just this
    }


    object TypeConst {
      def apply(tp: Type) = {
        if (tp =:= NullTp) NullConst
        else if (tp.isInstanceOf[SingletonType]) ValueConst.fromType(tp)
        else Const.unique(tp, new TypeConst(tp))
      }
      def unapply(c: TypeConst): Some[Type] = Some(c.tp)
    }

    // corresponds to a type test that does not imply any value-equality (well, except for outer checks, which we don't model yet)
    sealed class TypeConst(val tp: Type) extends Const {
      assert(!(tp =:= NullTp))
      private[this] val id: Int = Const.nextTypeId

      val wideTp = tp.widen

      override def toString = tp.toString //+"#"+ id
    }

    // p is a unique type or a constant value
    object ValueConst {
      def fromType(tp: Type) = {
        assert(tp.isInstanceOf[SingletonType])
        val toString = tp match {
          case ConstantType(c) => c.escapedStringValue
          case _ => tp.toString
        }
        Const.unique(tp, new ValueConst(tp, tp.widen, toString))
      }
      def apply(p: Tree) = {
        val tp = p.tpe.normalize
        if (tp =:= NullTp) NullConst
        else {
          val wideTp = {
            if (p.hasSymbol && p.symbol.isStable) tp.asSeenFrom(tp.prefix, p.symbol.owner).widen
            else tp.widen
          }

          val narrowTp =
            if (tp.isInstanceOf[SingletonType]) tp
            else p match {
              case Literal(c) =>
                if (c.tpe.typeSymbol == UnitClass) c.tpe
                else ConstantType(c)
              case p if p.symbol.isStable =>
                singleType(tp.prefix, p.symbol)
              case x =>
                // TODO: better type
                x.tpe.narrow
            }

          val toString =
            if (p.hasSymbol && p.symbol.isStable) p.symbol.name.toString // tp.toString
            else p.toString //+"#"+ id

          Const.unique(narrowTp, new ValueConst(narrowTp, checkableType(wideTp), toString)) // must make wide type checkable so that it is comparable to types from TypeConst
        }
      }
    }
    sealed class ValueConst(val tp: Type, val wideTp: Type, override val toString: String) extends Const {
      // patmatDebug("VC"+(tp, wideTp, toString))
      assert(!(tp =:= NullTp))
      private[this] val id: Int = Const.nextValueId
    }

    lazy val NullTp = ConstantType(Constant(null))
    case object NullConst extends Const {
      protected def tp     = NullTp
      protected def wideTp = NullTp

      def isValue = true
      override def toString = "null"
    }


    // turns a case (represented as a list of abstract tests)
    // into a proposition that is satisfiable if the case may match
    def symbolicCase(tests: List[Test], modelNull: Boolean = false): Prop = {
      def symbolic(t: Cond): Prop = t match {
        case AndCond(a, b) => And(symbolic(a), symbolic(b))
        case OrCond(a, b) => Or(symbolic(a), symbolic(b))
        case Top => True
        case Havoc => False
        case TypeCond(p, pt) => Eq(Var(p), TypeConst(checkableType(pt)))
        case EqualityCond(p, q) => Eq(Var(p), ValueConst(q))
        case NonNullCond(p) => if (!modelNull) True else Not(Eq(Var(p), NullConst))
      }

      val testsBeforeBody = tests.takeWhile(t => !t.treeMaker.isInstanceOf[BodyTreeMaker])
      /\(testsBeforeBody.map(t => symbolic(t.cond)))
    }

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
    def unreachableCase(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): Option[Int] = {
      // customize TreeMakersToConds (which turns a tree of tree makers into a more abstract DAG of tests)
      // when approximating the current case (which we hope is reachable), be optimistic about the unknowns
      object reachabilityApproximation extends TreeMakersToConds(prevBinder) {
        def makeCondOptimistic(tm: TreeMaker)(recurse: TreeMaker => Cond): Cond = tm match {
          // for unreachability, let's assume a guard always matches (unless we statically determined otherwise)
          // otherwise, a guarded case would be considered unreachable
          case GuardTreeMaker(guard) =>
            guard.tpe match {
              case ConstantType(Constant(false)) => Havoc // not the best name; however, symbolically, 'Havoc' becomes 'False'
              case _ => Top
            }
          // similar to a guard, user-defined extractors should not cause us to freak out
          // if we're not 100% sure it does not match (i.e., its result type is None or Constant(false) -- TODO),
          // let's stay optimistic and assume it does
          case ExtractorTreeMaker(_, _, _, _)
             | ProductExtractorTreeMaker(_, Some(_), _)  => Top
          // TODO: consider length-checks
          case _ =>
            makeCond(tm)(recurse)
        }

        // be pessimistic when approximating the prefix of the current case
        // we hope the prefix fails so that we might get to the current case, which we hope is not dead
        def makeCondPessimistic(tm: TreeMaker)(recurse: TreeMaker => Cond): Cond = makeCond(tm)(recurse)
      }

      val start = startTimer(patmatAnaReach)

      // use the same approximator so we share variables,
      // but need different conditions depending on whether we're conservatively looking for failure or success
      val testCasesOk   = reachabilityApproximation.approximateMatch(cases, reachabilityApproximation.makeCondOptimistic)
      val testCasesFail = reachabilityApproximation.approximateMatchAgain(cases, reachabilityApproximation.makeCondPessimistic)

      reachabilityApproximation.discard()
      prepareNewAnalysis()

      val propsCasesOk   = testCasesOk   map (t => symbolicCase(t, modelNull = true))
      val propsCasesFail = testCasesFail map (t => Not(symbolicCase(t, modelNull = true)))
      val (eqAxiomsFail, symbolicCasesFail) = removeVarEq(propsCasesFail, considerNull = true)
      val (eqAxiomsOk, symbolicCasesOk)     = removeVarEq(propsCasesOk,   considerNull = true)

      try {
        // most of the time eqAxiomsFail == eqAxiomsOk, but the different approximations might cause different variables to disapper in general
        val eqAxiomsCNF =
          if (eqAxiomsFail == eqAxiomsOk) eqFreePropToSolvable(eqAxiomsFail)
          else eqFreePropToSolvable(And(eqAxiomsFail, eqAxiomsOk))

        var prefix     = eqAxiomsCNF
        var prefixRest = symbolicCasesFail
        var current    = symbolicCasesOk
        var reachable  = true
        var caseIndex  = 0

  //      patmatDebug("reachability, vars:\n"+ ((propsCasesFail flatMap gatherVariables) map (_.describe) mkString ("\n")))
  //      patmatDebug("equality axioms:\n"+ cnfString(eqAxiomsCNF))

        // invariant (prefixRest.length == current.length) && (prefix.reverse ++ prefixRest == symbolicCasesFail)
        // termination: prefixRest.length decreases by 1
        while (prefixRest.nonEmpty && reachable) {
          val prefHead = prefixRest.head
          caseIndex += 1
          prefixRest = prefixRest.tail
          if (prefixRest.isEmpty) reachable = true
          else {
            prefix = andFormula(eqFreePropToSolvable(prefHead), prefix)
            current = current.tail
            val model = findModelFor(andFormula(eqFreePropToSolvable(current.head), prefix))

  //          patmatDebug("trying to reach:\n"+ cnfString(current.head) +"\nunder prefix:\n"+ cnfString(prefix))
  //          if (ok) patmatDebug("reached: "+ modelString(model))

            reachable = model ne NoModel
          }
        }

        stopTimer(patmatAnaReach, start)

        if (reachable) None else Some(caseIndex)
      } catch {
        case e : CNFBudgetExceeded =>
//          debugWarn(util.Position.formatMessage(prevBinder.pos, "Cannot check match for reachability", false))
//          e.printStackTrace()
          None // CNF budget exceeded
      }
    }

    // exhaustivity

    // make sure it's not a primitive, else (5: Byte) match { case 5 => ... } sees no Byte
    // TODO: domain of feasibly enumerable built-in types (enums, char?)
    def enumerateSubtypes(tp: Type): Option[List[Type]] =
      tp.typeSymbol match {
        case BooleanClass =>
          // patmatDebug("enum bool "+ tp)
          Some(List(ConstantType(Constant(true)), ConstantType(Constant(false))))
        // TODO case _ if tp.isTupleType => // recurse into component types
        case sym if !sym.isSealed || isPrimitiveValueClass(sym) =>
          // patmatDebug("enum unsealed "+ (tp, sym, sym.isSealed, isPrimitiveValueClass(sym)))
          None
        case sym =>
          val subclasses = (
            sym.sealedDescendants.toList sortBy (_.sealedSortName)
            // symbols which are both sealed and abstract need not be covered themselves, because
            // all of their children must be and they cannot otherwise be created.
            filterNot (x => x.isSealed && x.isAbstractClass && !isPrimitiveValueClass(x)))
          // patmatDebug("subclasses "+ (sym, subclasses))

          val tpApprox = typer.infer.approximateAbstracts(tp)
          val pre = tpApprox.prefix
          // valid subtypes are turned into checkable types, as we are entering the realm of the dynamic
          val validSubTypes = (subclasses flatMap {sym =>
              // have to filter out children which cannot match: see ticket #3683 for an example
              // compare to the fully known type `tp` (modulo abstract types),
              // so that we can rule out stuff like: sealed trait X[T]; class XInt extends X[Int] --> XInt not valid when enumerating X[String]
              // however, must approximate abstract types in
              val subTp       = appliedType(pre.memberType(sym), sym.typeParams.map(_ => WildcardType))
              val subTpApprox = typer.infer.approximateAbstracts(subTp) // TODO: needed?
              // patmatDebug("subtp"+(subTpApprox <:< tpApprox, subTpApprox, tpApprox))
              if (subTpApprox <:< tpApprox) Some(checkableType(subTp))
              else None
            })
          // patmatDebug("enum sealed "+ (tp, tpApprox) + " as "+ validSubTypes)
          Some(validSubTypes)
      }

    // approximate a type to the static type that is fully checkable at run time,
    // hiding statically known but dynamically uncheckable information using existential quantification
    // TODO: this is subject to the availability of TypeTags (since an abstract type with a type tag is checkable at run time)
    def checkableType(tp: Type): Type = {
      // TODO: this is extremely rough...
      object toCheckable extends TypeMap {
        def apply(tp: Type) = tp match {
          case TypeRef(pre, sym, a :: as) if sym ne ArrayClass =>
            // replace type args by existentials, since they can't be checked
            // TODO: when type tags are available, we will check -- when this is implemented, can we take that into account here?
            // TODO: don't reuse sym.typeParams, they have bounds (and those must not be considered)
            newExistentialType(sym.typeParams, sym.tpe).asSeenFrom(pre, sym.owner)
          case _ => mapOver(tp)
        }
      }
      val res = toCheckable(tp)
      // patmatDebug("checkable "+(tp, res))
      res
    }

    // a type is "uncheckable" (for exhaustivity) if we don't statically know its subtypes (i.e., it's unsealed)
    // we consider tuple types with at least one component of a checkable type as a checkable type
    def uncheckableType(tp: Type): Boolean = {
      @inline def tupleComponents(tp: Type) = tp.normalize.typeArgs
      val checkable = (
           (isTupleType(tp) && tupleComponents(tp).exists(tp => !uncheckableType(tp)))
        || enumerateSubtypes(tp).nonEmpty)
      // if (!checkable) patmatDebug("deemed uncheckable: "+ tp)
      !checkable
    }

    def exhaustive(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[String] = if (uncheckableType(prevBinder.info)) Nil else {
      // customize TreeMakersToConds (which turns a tree of tree makers into a more abstract DAG of tests)
      // - approximate the pattern `List()` (unapplySeq on List with empty length) as `Nil`,
      //   otherwise the common (xs: List[Any]) match { case List() => case x :: xs => } is deemed unexhaustive
      // - back off (to avoid crying exhaustive too often) when:
      //    - there are guards -->
      //    - there are extractor calls (that we can't secretly/soundly) rewrite
      val start = startTimer(patmatAnaExhaust)
      var backoff = false
      object exhaustivityApproximation extends TreeMakersToConds(prevBinder) {
        def makeCondExhaustivity(tm: TreeMaker)(recurse: TreeMaker => Cond): Cond = tm match {
          case p @ ExtractorTreeMaker(extractor, Some(lenCheck), testedBinder, _) =>
            p.checkedLength match {
              // pattern: `List()`  (interpret as `Nil`)
              // TODO: make it more general List(1, 2) => 1 :: 2 :: Nil
              case Some(0) if testedBinder.tpe.typeSymbol == ListClass => // extractor.symbol.owner == SeqFactory
                EqualityCond(binderToUniqueTree(p.prevBinder), unique(Ident(NilModule) setType NilModule.tpe))
              case _ =>
                backoff = true
                makeCond(tm)(recurse)
            }
          case ExtractorTreeMaker(_, _, _, _) =>
// patmatDebug("backing off due to "+ tm)
            backoff = true
            makeCond(tm)(recurse)
          case GuardTreeMaker(guard) =>
            guard.tpe match {
              case ConstantType(Constant(true)) => Top
              case ConstantType(Constant(false)) => Havoc
              case _ =>
// patmatDebug("can't statically interpret guard: "+(guard, guard.tpe))
                backoff = true
                Havoc
            }
          case _ =>
            makeCond(tm)(recurse)
        }
      }

      val tests = exhaustivityApproximation.approximateMatch(cases, exhaustivityApproximation.makeCondExhaustivity)

      if (backoff) Nil else {
        val prevBinderTree = exhaustivityApproximation.binderToUniqueTree(prevBinder)

        exhaustivityApproximation.discard()
        prepareNewAnalysis()

        val symbolicCases = tests map (symbolicCase(_, modelNull = false))


        // TODO: null tests generate too much noise, so disabled them -- is there any way to bring them back?
        // assuming we're matching on a non-null scrutinee (prevBinder), when does the match fail?
        // val nonNullScrutineeCond =
        //   assume non-null for all the components of the tuple we're matching on (if we're matching on a tuple)
        //   if (isTupleType(prevBinder.tpe))
        //     prevBinder.tpe.typeArgs.mapWithIndex{case (_, i) => NonNullCond(codegen.tupleSel(prevBinderTree)(i))}.reduceLeft(AndCond)
        //   else
        //     NonNullCond(prevBinderTree)
        // val matchFails = And(symbolic(nonNullScrutineeCond), Not(symbolicCases reduceLeft (Or(_, _))))

        // when does the match fail?
        val matchFails = Not(\/(symbolicCases))

  // debug output:
        // patmatDebug("analysing:")
        // showTreeMakers(cases)
        // showTests(tests)
        //
        // val vars = gatherVariables(matchFails)
        // patmatDebug("\nvars:\n"+ (vars map (_.describe) mkString ("\n")))
        //
        // patmatDebug("\nmatchFails as CNF:\n"+ cnfString(propToSolvable(matchFails)))

        try {
          // find the models (under which the match fails)
          val matchFailModels = findAllModelsFor(propToSolvable(matchFails))

          val scrutVar = Var(prevBinderTree)
          val counterExamples = matchFailModels.map(modelToCounterExample(scrutVar))

          val pruned = CounterExample.prune(counterExamples).map(_.toString).sorted

          stopTimer(patmatAnaExhaust, start)
          pruned
        } catch {
          case e : CNFBudgetExceeded =>
            // patmatDebug(util.Position.formatMessage(prevBinder.pos, "Cannot check match for exhaustivity", false))
            // e.printStackTrace()
            Nil // CNF budget exceeded
        }
      }
    }

    object CounterExample {
      def prune(examples: List[CounterExample]): List[CounterExample] = {
        val distinct = examples.filterNot(_ == NoExample).toSet
        distinct.filterNot(ce => distinct.exists(other => (ce ne other) && ce.coveredBy(other))).toList
      }
    }

    // a way to construct a value that will make the match fail: a constructor invocation, a constant, an object of some type)
    class CounterExample {
      protected[SymbolicMatchAnalysis] def flattenConsArgs: List[CounterExample] = Nil
      def coveredBy(other: CounterExample): Boolean = this == other || other == WildcardExample
    }
    case class ValueExample(c: ValueConst) extends CounterExample { override def toString = c.toString }
    case class TypeExample(c: Const)  extends CounterExample { override def toString = "(_ : "+ c +")" }
    case class NegativeExample(nonTrivialNonEqualTo: List[Const]) extends CounterExample {
      // require(nonTrivialNonEqualTo.nonEmpty, nonTrivialNonEqualTo)
      override def toString = {
        val negation =
          if (nonTrivialNonEqualTo.tail.isEmpty) nonTrivialNonEqualTo.head.toString
          else nonTrivialNonEqualTo.map(_.toString).sorted.mkString("in (", ", ", ")")
        "<not "+ negation +">"
      }
    }
    case class ListExample(ctorArgs: List[CounterExample]) extends CounterExample {
      protected[SymbolicMatchAnalysis] override def flattenConsArgs: List[CounterExample] = ctorArgs match {
        case hd :: tl :: Nil => hd :: tl.flattenConsArgs
        case _ => Nil
      }
      protected[SymbolicMatchAnalysis] lazy val elems = flattenConsArgs

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case other@ListExample(_) =>
            this == other || ((elems.length == other.elems.length) && (elems zip other.elems).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }

      override def toString = elems.mkString("List(", ", ", ")")
    }
    case class TupleExample(ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = ctorArgs.mkString("(", ", ", ")")

      override def coveredBy(other: CounterExample): Boolean =
        other match {
          case TupleExample(otherArgs) =>
            this == other || ((ctorArgs.length == otherArgs.length) && (ctorArgs zip otherArgs).forall{case (a, b) => a coveredBy b})
          case _ => super.coveredBy(other)
        }
    }
    case class ConstructorExample(cls: Symbol, ctorArgs: List[CounterExample]) extends CounterExample {
      override def toString = cls.decodedName + (if (cls.isModuleClass) "" else ctorArgs.mkString("(", ", ", ")"))
    }

    case object WildcardExample extends CounterExample { override def toString = "_" }
    case object NoExample extends CounterExample { override def toString = "??" }

    @inline def modelToVarAssignment(model: Model): Map[Var, (Seq[Const], Seq[Const])] =
      model.toSeq.groupBy{f => f match {case (sym, value) => sym.variable} }.mapValues{ xs =>
        val (trues, falses) = xs.partition(_._2)
        (trues map (_._1.const), falses map (_._1.const))
        // should never be more than one value in trues...
      }

    def varAssignmentString(varAssignment: Map[Var, (Seq[Const], Seq[Const])]) =
      varAssignment.toSeq.sortBy(_._1.toString).map { case (v, (trues, falses)) =>
         val assignment = "== "+ (trues mkString("(", ", ", ")")) +"  != ("+ (falses mkString(", ")) +")"
         v +"(="+ v.path +": "+ v.domainTp +") "+ assignment
       }.mkString("\n")

    def modelString(model: Model) = varAssignmentString(modelToVarAssignment(model))

    // return constructor call when the model is a true counter example
    // (the variables don't take into account type information derived from other variables,
    //  so, naively, you might try to construct a counter example like _ :: Nil(_ :: _, _ :: _),
    //  since we didn't realize the tail of the outer cons was a Nil)
    def modelToCounterExample(scrutVar: Var)(model: Model): CounterExample = {
      // x1 = ...
      // x1.hd = ...
      // x1.tl = ...
      // x1.hd.hd = ...
      // ...
      val varAssignment = modelToVarAssignment(model)

      // patmatDebug("var assignment for model "+ model +":\n"+ varAssignmentString(varAssignment))

      // chop a path into a list of symbols
      def chop(path: Tree): List[Symbol] = path match {
        case Ident(_) => List(path.symbol)
        case Select(pre, name) => chop(pre) :+ path.symbol
        case _ => // patmatDebug("don't know how to chop "+ path)
          Nil
      }

      // turn the variable assignments into a tree
      // the root is the scrutinee (x1), edges are labelled by the fields that are assigned
      // a node is a variable example (which is later turned into a counter example)
      object VariableAssignment {
        private def findVar(path: List[Symbol]) = path match {
          case List(root) if root == scrutVar.path.symbol => Some(scrutVar)
          case _ => varAssignment.find{case (v, a) => chop(v.path) == path}.map(_._1)
        }

        private val uniques = new collection.mutable.HashMap[Var, VariableAssignment]
        private def unique(variable: Var): VariableAssignment =
          uniques.getOrElseUpdate(variable, {
            val (eqTo, neqTo) = varAssignment.getOrElse(variable, (Nil, Nil)) // TODO
            VariableAssignment(variable, eqTo.toList, neqTo.toList, HashMap.empty)
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
              outerCtor.fields(field) = newCtor
            }
            newCtor
          }
        }
      }

      // node in the tree that describes how to construct a counter-example
      case class VariableAssignment(variable: Var, equalTo: List[Const], notEqualTo: List[Const], fields: collection.mutable.Map[Symbol, VariableAssignment]) {
        // need to prune since the model now incorporates all super types of a constant (needed for reachability)
        private lazy val prunedEqualTo = equalTo filterNot (subsumed => equalTo.exists(better => (better ne subsumed) && (better implies subsumed)))
        private lazy val ctor       = (prunedEqualTo match { case List(TypeConst(tp)) => tp case _ => variable.domainTp }).typeSymbol.primaryConstructor
        private lazy val ctorParams = if (ctor == NoSymbol || ctor.paramss.isEmpty) Nil else ctor.paramss.head
        private lazy val cls        = if (ctor == NoSymbol) NoSymbol else ctor.owner
        private lazy val caseFieldAccs = if (cls == NoSymbol) Nil else cls.caseFieldAccessors


        def allFieldAssignmentsLegal: Boolean =
          (fields.keySet subsetOf caseFieldAccs.toSet) && fields.values.forall(_.allFieldAssignmentsLegal)

        private lazy val nonTrivialNonEqualTo = notEqualTo.filterNot{c => c.isAny }

        // NoExample if the constructor call is ill-typed
        // (thus statically impossible -- can we incorporate this into the formula?)
        // beBrief is used to suppress negative information nested in tuples -- it tends to get too noisy
        def toCounterExample(beBrief: Boolean = false): CounterExample =
          if (!allFieldAssignmentsLegal) NoExample
          else {
            // patmatDebug("describing "+ (variable, equalTo, notEqualTo, fields, cls, allFieldAssignmentsLegal))
            val res = prunedEqualTo match {
              // a definite assignment to a value
              case List(eq: ValueConst) if fields.isEmpty => ValueExample(eq)

              // constructor call
              // or we did not gather any information about equality but we have information about the fields
              //  --> typical example is when the scrutinee is a tuple and all the cases first unwrap that tuple and only then test something interesting
              case _ if cls != NoSymbol &&
                        (  prunedEqualTo.nonEmpty
                        || (fields.nonEmpty && !isPrimitiveValueClass(cls) && prunedEqualTo.isEmpty && notEqualTo.isEmpty)) =>

                @inline def args(brevity: Boolean = beBrief) = {
                  // figure out the constructor arguments from the field assignment
                  val argLen = (caseFieldAccs.length min ctorParams.length)

                  (0 until argLen).map(i => fields.get(caseFieldAccs(i)).map(_.toCounterExample(brevity)) getOrElse WildcardExample).toList
                }

                cls match {
                  case ConsClass               => ListExample(args())
                  case _ if isTupleSymbol(cls) => TupleExample(args(true))
                  case _ => ConstructorExample(cls, args())
                }

              // a definite assignment to a type
              case List(eq) if fields.isEmpty => TypeExample(eq)

              // negative information
              case Nil if nonTrivialNonEqualTo.nonEmpty =>
                // negation tends to get pretty verbose
                if (beBrief) WildcardExample else NegativeExample(nonTrivialNonEqualTo)

              // not a valid counter-example, possibly since we have a definite type but there was a field mismatch
              // TODO: improve reasoning -- in the mean time, a false negative is better than an annoying false positive
              case _ => NoExample
            }
            // patmatDebug("described as: "+ res)
            res
          }

        override def toString = toCounterExample().toString
      }

      // slurp in information from other variables
      varAssignment.keys.foreach{ v => if (v != scrutVar) VariableAssignment(v) }

      // this is the variable we want a counter example for
      VariableAssignment(scrutVar).toCounterExample()
    }
  }

////
  trait CommonSubconditionElimination extends TreeMakerApproximation { self: OptimizedCodegen =>
    /** a flow-sensitive, generalised, common sub-expression elimination
     * reuse knowledge from performed tests
     * the only sub-expressions we consider are the conditions and results of the three tests (type, type&equality, equality)
     * when a sub-expression is shared, it is stored in a mutable variable
     * the variable is floated up so that its scope includes all of the program that shares it
     * we generalize sharing to implication, where b reuses a if a => b and priors(a) => priors(b) (the priors of a sub expression form the path through the decision tree)
     */
    def doCSE(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[List[TreeMaker]] = {
      val testss = approximateMatch(prevBinder, cases)

      // interpret:
      val dependencies = new collection.mutable.LinkedHashMap[Test, Set[Cond]]
      val tested = new collection.mutable.HashSet[Cond]

      def storeDependencies(test: Test) = {
        val cond = test.cond

        def simplify(c: Cond): Set[Cond] = c match {
          case AndCond(a, b) => simplify(a) ++ simplify(b)
          case OrCond(_, _) => Set(Havoc) // TODO: supremum?
          case NonNullCond(_) => Set(Top) // not worth remembering
          case _ => Set(c)
        }
        val conds = simplify(cond)

        if (conds(Havoc)) false // stop when we encounter a havoc
        else {
          val nonTrivial = conds filterNot (_ == Top)
          if (nonTrivial nonEmpty) {
            tested ++= nonTrivial

            // is there an earlier test that checks our condition and whose dependencies are implied by ours?
            dependencies find {
              case (priorTest, deps) =>
                ((simplify(priorTest.cond) == nonTrivial) || // our conditions are implied by priorTest if it checks the same thing directly
                 (nonTrivial subsetOf deps)                  // or if it depends on a superset of our conditions
                ) && (deps subsetOf tested)             // the conditions we've tested when we are here in the match satisfy the prior test, and hence what it tested
            } foreach {
              case (priorTest, _) =>
                // if so, note the dependency in both tests
                priorTest registerReuseBy test
            }

            dependencies(test) = tested.toSet // copies
          }
          true
        }
      }


      testss foreach { tests =>
        tested.clear()
        tests dropWhile storeDependencies
      }
      // patmatDebug("dependencies: "+ dependencies)

      // find longest prefix of tests that reuse a prior test, and whose dependent conditions monotonically increase
      // then, collapse these contiguous sequences of reusing tests
      // store the result of the final test and the intermediate results in hoisted mutable variables (TODO: optimize: don't store intermediate results that aren't used)
      // replace each reference to a variable originally bound by a collapsed test by a reference to the hoisted variable
      val reused = new collection.mutable.HashMap[TreeMaker, ReusedCondTreeMaker]
      var okToCall = false
      val reusedOrOrig = (tm: TreeMaker) => {assert(okToCall); reused.getOrElse(tm, tm)}

      // maybe collapse: replace shared prefix of tree makers by a ReusingCondTreeMaker
      // once this has been computed, we'll know which tree makers are reused,
      // and we'll replace those by the ReusedCondTreeMakers we've constructed (and stored in the reused map)
      val collapsed = testss map { tests =>
        // map tests to the equivalent list of treemakers, replacing shared prefixes by a reusing treemaker
        // if there's no sharing, simply map to the tree makers corresponding to the tests
        var currDeps = Set[Cond]()
        val (sharedPrefix, suffix) = tests span { test =>
          (test.cond eq Top) || (for(
              reusedTest <- test.reuses;
              nextDeps <- dependencies.get(reusedTest);
              diff <- (nextDeps -- currDeps).headOption;
              _ <- Some(currDeps = nextDeps))
                yield diff).nonEmpty
        }

        val collapsedTreeMakers =
          if (sharedPrefix.isEmpty) None
          else { // even sharing prefixes of length 1 brings some benefit (overhead-percentage for compiler: 26->24%, lib: 19->16%)
            for (test <- sharedPrefix; reusedTest <- test.reuses) reusedTest.treeMaker match {
              case reusedCTM: CondTreeMaker => reused(reusedCTM) = ReusedCondTreeMaker(reusedCTM)
              case _ =>
            }

            // patmatDebug("sharedPrefix: "+ sharedPrefix)
            // if the shared prefix contains interesting conditions (!= Top)
            // and the last of such interesting shared conditions reuses another treemaker's test
            // replace the whole sharedPrefix by a ReusingCondTreeMaker
            for (lastShared <- sharedPrefix.reverse.dropWhile(_.cond eq Top).headOption;
                 lastReused <- lastShared.reuses)
              yield ReusingCondTreeMaker(sharedPrefix, reusedOrOrig) :: suffix.map(_.treeMaker)
          }

        collapsedTreeMakers getOrElse tests.map(_.treeMaker) // sharedPrefix need not be empty (but it only contains Top-tests, which are dropped above)
      }
      okToCall = true // TODO: remove (debugging)

      // replace original treemakers that are reused (as determined when computing collapsed),
      // by ReusedCondTreeMakers
      val reusedMakers = collapsed mapConserve (_ mapConserve reusedOrOrig)
// patmatDebug("after CSE:")
//      showTreeMakers(reusedMakers)
      reusedMakers
    }

    object ReusedCondTreeMaker {
      def apply(orig: CondTreeMaker) = new ReusedCondTreeMaker(orig.prevBinder, orig.nextBinder, orig.cond, orig.res, orig.pos)
    }
    class ReusedCondTreeMaker(prevBinder: Symbol, val nextBinder: Symbol, cond: Tree, res: Tree, val pos: Position) extends TreeMaker { import CODE._
      lazy val localSubstitution        = Substitution(List(prevBinder), List(CODE.REF(nextBinder)))
      lazy val storedCond               = freshSym(pos, BooleanClass.tpe, "rc") setFlag MUTABLE
      lazy val treesToHoist: List[Tree] = {
        nextBinder setFlag MUTABLE
        List(storedCond, nextBinder) map { b => VAL(b) === codegen.mkZero(b.info) }
      }

      // TODO: finer-grained duplication
      def chainBefore(next: Tree)(casegen: Casegen): Tree = // assert(codegen eq optimizedCodegen)
        atPos(pos)(casegen.asInstanceOf[optimizedCodegen.OptimizedCasegen].flatMapCondStored(cond, storedCond, res, nextBinder, substitution(next).duplicate))

      override def toString = "Memo"+(nextBinder.name, storedCond.name, cond, res, substitution)
    }

    case class ReusingCondTreeMaker(sharedPrefix: List[Test], toReused: TreeMaker => TreeMaker) extends TreeMaker { import CODE._
      val pos = sharedPrefix.last.treeMaker.pos

      lazy val localSubstitution = {
        // replace binder of each dropped treemaker by corresponding binder bound by the most recent reused treemaker
        var mostRecentReusedMaker: ReusedCondTreeMaker = null
        def mapToStored(droppedBinder: Symbol) = if (mostRecentReusedMaker eq null) Nil else List((droppedBinder, REF(mostRecentReusedMaker.nextBinder)))
        val (from, to) = sharedPrefix.flatMap { dropped =>
          dropped.reuses.map(test => toReused(test.treeMaker)).foreach {
            case reusedMaker: ReusedCondTreeMaker =>
              mostRecentReusedMaker = reusedMaker
            case _ =>
          }

          // TODO: have super-trait for retrieving the variable that's operated on by a tree maker
          // and thus assumed in scope, either because it binds it or because it refers to it
          dropped.treeMaker match {
            case dropped: FunTreeMaker =>
              mapToStored(dropped.nextBinder)
            case _ => Nil
          }
        }.unzip
        val rerouteToReusedBinders = Substitution(from, to)

        val collapsedDroppedSubst = sharedPrefix map (t => (toReused(t.treeMaker).substitution))

        collapsedDroppedSubst.foldLeft(rerouteToReusedBinders)(_ >> _)
      }

      lazy val lastReusedTreeMaker = sharedPrefix.reverse.flatMap(tm => tm.reuses map (test => toReused(test.treeMaker))).collectFirst{case x: ReusedCondTreeMaker => x}.head

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        // TODO: finer-grained duplication -- MUST duplicate though, or we'll get VerifyErrors since sharing trees confuses lambdalift,
        // and in its confusion it emits illegal casts (diagnosed by Grzegorz: checkcast T ; invokevirtual S.m, where T not a subtype of S)
        casegen.ifThenElseZero(REF(lastReusedTreeMaker.storedCond), substitution(next).duplicate)
      }
      override def toString = "R"+(lastReusedTreeMaker.storedCond.name, substitution)
    }
  }


  //// DCE
  trait DeadCodeElimination extends TreeMakers { self: CodegenCore =>
    // TODO: non-trivial dead-code elimination
    // e.g., the following match should compile to a simple instanceof:
    //   case class Ident(name: String)
    //   for (Ident(name) <- ts) println(name)
    def doDCE(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type): List[List[TreeMaker]] = {
      // do minimal DCE
      cases
    }
  }

  //// SWITCHES -- TODO: operate on Tests rather than TreeMakers
  trait SwitchEmission extends TreeMakers with OptimizedMatchMonadInterface { self: CodegenCore =>
    abstract class SwitchMaker {
      abstract class SwitchableTreeMakerExtractor { def unapply(x: TreeMaker): Option[Tree] }
      val SwitchableTreeMaker: SwitchableTreeMakerExtractor

      def alternativesSupported: Boolean

      def isDefault(x: CaseDef): Boolean
      def defaultSym: Symbol
      def defaultBody: Tree
      def defaultCase(scrutSym: Symbol = defaultSym, body: Tree = defaultBody): CaseDef

      private def sequence[T](xs: List[Option[T]]): Option[List[T]] =
        if (xs exists (_.isEmpty)) None else Some(xs.flatten)

      // empty list ==> failure
      def apply(cases: List[(Symbol, List[TreeMaker])], pt: Type): List[CaseDef] = {
        val caseDefs = cases map { case (scrutSym, makers) =>
          makers match {
            // default case
            case (btm@BodyTreeMaker(body, _)) :: Nil =>
              Some(defaultCase(scrutSym, btm.substitution(body)))
            // constant (or typetest for typeSwitch)
            case SwitchableTreeMaker(pattern) :: (btm@BodyTreeMaker(body, _)) :: Nil =>
              Some(CaseDef(pattern, EmptyTree, btm.substitution(body)))
            // alternatives
            case AlternativesTreeMaker(_, altss, _) :: (btm@BodyTreeMaker(body, _)) :: Nil if alternativesSupported =>
              val casePatterns = altss map {
                case SwitchableTreeMaker(pattern) :: Nil =>
                  Some(pattern)
                case _ =>
                  None
              }

              sequence(casePatterns) map { patterns =>
                val substedBody  = btm.substitution(body)
                CaseDef(Alternative(patterns), EmptyTree, substedBody)
              }
            case _ => // patmatDebug("can't emit switch for "+ makers)
              None //failure (can't translate pattern to a switch)
          }
        }

        (for(
          caseDefs <- sequence(caseDefs)) yield
            if (caseDefs exists isDefault) caseDefs
            else {
              caseDefs :+ defaultCase()
            }
        ) getOrElse Nil
      }
    }

    class RegularSwitchMaker(scrutSym: Symbol, matchFailGenOverride: Option[Tree => Tree]) extends SwitchMaker {
      val switchableTpe = Set(ByteClass.tpe, ShortClass.tpe, IntClass.tpe, CharClass.tpe)
      val alternativesSupported = true

      object SwitchablePattern { def unapply(pat: Tree): Option[Tree] = pat match {
        case Literal(const@Constant((_: Byte ) | (_: Short) | (_: Int  ) | (_: Char ))) =>
          Some(Literal(Constant(const.intValue))) // TODO: Java 7 allows strings in switches
        case _ => None
      }}

      object SwitchableTreeMaker extends SwitchableTreeMakerExtractor {
        def unapply(x: TreeMaker): Option[Tree] = x match {
          case EqualityTestTreeMaker(_, SwitchablePattern(const), _) => Some(const)
          case _ => None
        }
      }

      def isDefault(x: CaseDef): Boolean = x match {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, _) => true
        case _ => false
      }

      def defaultSym: Symbol = scrutSym
      def defaultBody: Tree  = { import CODE._; matchFailGenOverride map (gen => gen(REF(scrutSym))) getOrElse MATCHERROR(REF(scrutSym)) }
      def defaultCase(scrutSym: Symbol = defaultSym, body: Tree = defaultBody): CaseDef = { import CODE._; atPos(body.pos) {
        DEFAULT ==> body
      }}
    }

    override def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree]): Option[Tree] = { import CODE._
      val regularSwitchMaker = new RegularSwitchMaker(scrutSym, matchFailGenOverride)
      // TODO: if patterns allow switch but the type of the scrutinee doesn't, cast (type-test) the scrutinee to the corresponding switchable type and switch on the result
      if (regularSwitchMaker.switchableTpe(scrutSym.tpe)) {
        val caseDefsWithDefault = regularSwitchMaker(cases map {c => (scrutSym, c)}, pt)
        if (caseDefsWithDefault isEmpty) None // not worth emitting a switch.
        else {
          // match on scrutSym -- converted to an int if necessary -- not on scrut directly (to avoid duplicating scrut)
          val scrutToInt: Tree =
            if (scrutSym.tpe =:= IntClass.tpe) REF(scrutSym)
            else (REF(scrutSym) DOT (nme.toInt))
          Some(BLOCK(
            VAL(scrutSym) === scrut,
            Match(scrutToInt, caseDefsWithDefault) // a switch
          ))
        }
      } else None
    }

    // for the catch-cases in a try/catch
    private object typeSwitchMaker extends SwitchMaker {
      def switchableTpe(tp: Type) = true
      val alternativesSupported = false // TODO: needs either back-end support of flattening of alternatives during typers

      // TODO: there are more treemaker-sequences that can be handled by type tests
      // analyze the result of approximateTreeMaker rather than the TreeMaker itself
      object SwitchableTreeMaker extends SwitchableTreeMakerExtractor {
        def unapply(x: TreeMaker): Option[Tree] = x match {
          case tm@TypeTestTreeMaker(_, _, pt, _) if tm.isPureTypeTest => //  -- TODO: use this if binder does not occur in the body
            Some(Bind(tm.nextBinder, Typed(Ident(nme.WILDCARD), TypeTree(pt)) /* not used by back-end */))
          case _ =>
            None
        }
      }

      def isDefault(x: CaseDef): Boolean = x match {
        case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe)          => true
        case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe) => true
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, _)                                                          => true
        case _ => false
      }

      lazy val defaultSym: Symbol = freshSym(NoPosition, ThrowableClass.tpe)
      def defaultBody: Tree       = Throw(CODE.REF(defaultSym))
      def defaultCase(scrutSym: Symbol = defaultSym, body: Tree = defaultBody): CaseDef = { import CODE._; atPos(body.pos) {
        CASE (Bind(scrutSym, Typed(Ident(nme.WILDCARD), TypeTree(ThrowableClass.tpe)))) ==> body
      }}
    }

    // TODO: drop null checks
    override def emitTypeSwitch(bindersAndCases: List[(Symbol, List[TreeMaker])], pt: Type): Option[List[CaseDef]] = {
      val caseDefsWithDefault = typeSwitchMaker(bindersAndCases, pt)
      if (caseDefsWithDefault isEmpty) None
      else Some(caseDefsWithDefault)
    }
  }

  trait OptimizedMatchMonadInterface extends MatchMonadInterface {
    override def inMatchMonad(tp: Type): Type = optionType(tp)
    override def pureType(tp: Type): Type     = tp
    override protected def matchMonadSym      = OptionClass
  }

  trait OptimizedCodegen extends CodegenCore with TypedSubstitution with OptimizedMatchMonadInterface {
    override def codegen: AbsCodegen = optimizedCodegen

    // trait AbsOptimizedCodegen extends AbsCodegen {
    //   def flatMapCondStored(cond: Tree, condSym: Symbol, res: Tree, nextBinder: Symbol, next: Tree): Tree
    // }
    // def optimizedCodegen: AbsOptimizedCodegen

    // when we know we're targetting Option, do some inlining the optimizer won't do
    // for example, `o.flatMap(f)` becomes `if(o == None) None else f(o.get)`, similarly for orElse and guard
    //   this is a special instance of the advanced inlining optimization that takes a method call on
    //   an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    object optimizedCodegen extends CommonCodegen { import CODE._

      /** Inline runOrElse and get rid of Option allocations
       *
       * runOrElse(scrut: scrutTp)(matcher): resTp = matcher(scrut) getOrElse ${catchAll(`scrut`)}
       * the matcher's optional result is encoded as a flag, keepGoing, where keepGoing == true encodes result.isEmpty,
       * if keepGoing is false, the result Some(x) of the naive translation is encoded as matchRes == x
       */
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree = {
        val matchEnd = NoSymbol.newLabel(freshName("matchEnd"), NoPosition) setFlag SYNTH_CASE
        val matchRes = NoSymbol.newValueParameter(newTermName("x"), NoPosition, SYNTHETIC) setInfo restpe.withoutAnnotations //
        matchEnd setInfo MethodType(List(matchRes), restpe)

        def newCaseSym = NoSymbol.newLabel(freshName("case"), NoPosition) setInfo MethodType(Nil, restpe) setFlag SYNTH_CASE
        var nextCase = newCaseSym
        def caseDef(mkCase: Casegen => Tree): Tree = {
          val currCase = nextCase
          nextCase = newCaseSym
          val casegen = new OptimizedCasegen(matchEnd, nextCase, restpe)
          LabelDef(currCase, Nil, mkCase(casegen))
        }

        def catchAll = matchFailGen map { matchFailGen =>
          val scrutRef = if(scrutSym ne NoSymbol) REF(scrutSym) else EmptyTree // for alternatives
          // must jump to matchEnd, use result generated by matchFailGen (could be `FALSE` for isDefinedAt)
          LabelDef(nextCase, Nil, matchEnd APPLY (matchFailGen(scrutRef)))
          // don't cast the arg to matchEnd when using PartialFun synth in uncurry, since it won't detect the throw (see gen.withDefaultCase)
          // the cast is necessary when using typedMatchAnonFun-style PartialFun synth:
          // (_asInstanceOf(matchFailGen(scrutRef), restpe))
        } toList
        // catchAll.isEmpty iff no synthetic default case needed (the (last) user-defined case is a default)
        // if the last user-defined case is a default, it will never jump to the next case; it will go immediately to matchEnd

        // the generated block is taken apart in TailCalls under the following assumptions
          // the assumption is once we encounter a case, the remainder of the block will consist of cases
          // the prologue may be empty, usually it is the valdef that stores the scrut
          // val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])

        // scrutSym == NoSymbol when generating an alternatives matcher
        val scrutDef = if(scrutSym ne NoSymbol) List(VAL(scrutSym)  === scrut) else Nil // for alternatives
        Block(
          scrutDef ++ (cases map caseDef) ++ catchAll,
          LabelDef(matchEnd, List(matchRes), REF(matchRes))
        )
      }

      class OptimizedCasegen(matchEnd: Symbol, nextCase: Symbol, restpe: Type) extends CommonCodegen with Casegen {
        def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree =
          optimizedCodegen.matcher(scrut, scrutSym, restpe)(cases, matchFailGen)

        // only used to wrap the RHS of a body
        // res: T
        // returns MatchMonad[T]
        def one(res: Tree): Tree = matchEnd APPLY (_asInstanceOf(res, restpe)) // need cast for GADT magic
        protected def zero: Tree = nextCase APPLY ()

        // prev: MatchMonad[T]
        // b: T
        // next: MatchMonad[U]
        // returns MatchMonad[U]
        def flatMap(prev: Tree, b: Symbol, next: Tree): Tree = {
          val tp      = inMatchMonad(b.tpe)
          val prevSym = freshSym(prev.pos, tp, "o")
          val isEmpty = tp member vpmName.isEmpty
          val get     = tp member vpmName.get

          BLOCK(
            VAL(prevSym) === prev,
            // must be isEmpty and get as we don't control the target of the call (prev is an extractor call)
            ifThenElseZero(NOT(prevSym DOT isEmpty), Substitution(b, prevSym DOT get)(next))
          )
        }

        // cond: Boolean
        // res: T
        // nextBinder: T
        // next == MatchMonad[U]
        // returns MatchMonad[U]
        def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree =
          ifThenElseZero(cond, BLOCK(
            VAL(nextBinder) === res,
            next
          ))

        // guardTree: Boolean
        // next: MatchMonad[T]
        // returns MatchMonad[T]
        def flatMapGuard(guardTree: Tree, next: Tree): Tree =
          ifThenElseZero(guardTree, next)

        def flatMapCondStored(cond: Tree, condSym: Symbol, res: Tree, nextBinder: Symbol, next: Tree): Tree =
          ifThenElseZero(cond, BLOCK(
            condSym    === TRUE_typed,
            nextBinder === res,
            next
          ))
      }

    }
  }


  trait MatchOptimizations extends CommonSubconditionElimination
                              with DeadCodeElimination
                              with SwitchEmission
                              with OptimizedCodegen
                              with SymbolicMatchAnalysis
                              with DPLLSolver { self: TreeMakers =>
    override def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, unchecked: Boolean): (List[List[TreeMaker]], List[Tree]) = {
      if (!unchecked) {
        unreachableCase(prevBinder, cases, pt) foreach { caseIndex =>
          typer.context.unit.warning(cases(caseIndex).last.pos, "unreachable code")
        }
      }
      val counterExamples = if (unchecked) Nil else exhaustive(prevBinder, cases, pt)
      if (counterExamples.nonEmpty) {
        val ceString =
          if (counterExamples.tail.isEmpty) "input: " + counterExamples.head
          else "inputs: " + counterExamples.mkString(", ")

        typer.context.unit.warning(prevBinder.pos, "match may not be exhaustive.\nIt would fail on the following "+ ceString)
      }

      val optCases = doCSE(prevBinder, doDCE(prevBinder, cases, pt), pt)
      val toHoist = (
        for (treeMakers <- optCases)
          yield treeMakers.collect{case tm: ReusedCondTreeMaker => tm.treesToHoist}
        ).flatten.flatten.toList
      (optCases, toHoist)
    }
  }
}
