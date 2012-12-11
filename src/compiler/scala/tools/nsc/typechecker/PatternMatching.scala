/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package typechecker

import symtab._
import Flags.{MUTABLE, METHOD, LABEL, SYNTHETIC, ARTIFACT}
import scala.language.postfixOps
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.Types

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
  *  - DCE (on irrefutable patterns)
  *  - update spec and double check it's implemented correctly (see TODO's)
  *
  * (longer-term) TODO:
  *  - user-defined unapplyProd
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  *  - recover exhaustivity/unreachability of user-defined extractors by partitioning the types they match on using an HList or similar type-level structure
  */
trait PatternMatching extends Transform with TypingTransformers with ast.TreeDSL {   // self: Analyzer =>
  import Statistics._
  import PatternMatchingStats._

  val global: Global               // need to repeat here because otherwise last mixin defines global as
                                   // SymbolTable. If we had DOT this would not be an issue
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  val phaseName: String = "patmat"

  // TODO: the inliner fails to inline the closures to patmatDebug
  object debugging {
    val printPatmat = settings.Ypatmatdebug.value
    @inline final def patmatDebug(s: => String) = if (printPatmat) println(s)
  }
  import debugging.patmatDebug

  // to govern how much time we spend analyzing matches for unreachability/exhaustivity
  object AnalysisBudget {
    import scala.tools.cmd.FromString.IntFromString
    val max = sys.props.get("scalac.patmat.analysisBudget").collect(IntFromString.orElse{case "off" => Integer.MAX_VALUE}).getOrElse(256)

    abstract class Exception extends RuntimeException("CNF budget exceeded") {
      val advice: String
      def warn(pos: Position, kind: String) = currentUnit.uncheckedWarning(pos, s"Cannot check match for $kind.\n$advice")
    }

    object exceeded extends Exception {
      val advice = s"(The analysis required more space than allowed. Please try with scalac -Dscalac.patmat.analysisBudget=${AnalysisBudget.max*2} or -Dscalac.patmat.analysisBudget=off.)"
    }
  }

  def newTransformer(unit: CompilationUnit): Transformer =
    if (opt.virtPatmat) new MatchTransformer(unit)
    else noopTransformer

  // duplicated from CPSUtils (avoid dependency from compiler -> cps plugin...)
  private lazy val MarkerCPSAdaptPlus  = rootMirror.getClassIfDefined("scala.util.continuations.cpsPlus")
  private lazy val MarkerCPSAdaptMinus = rootMirror.getClassIfDefined("scala.util.continuations.cpsMinus")
  private lazy val MarkerCPSSynth      = rootMirror.getClassIfDefined("scala.util.continuations.cpsSynth")
  private lazy val stripTriggerCPSAnns = List(MarkerCPSSynth, MarkerCPSAdaptMinus, MarkerCPSAdaptPlus)
  private lazy val MarkerCPSTypes      = rootMirror.getClassIfDefined("scala.util.continuations.cpsParam")
  private lazy val strippedCPSAnns     = MarkerCPSTypes :: stripTriggerCPSAnns
  private def removeCPSAdaptAnnotations(tp: Type) = tp filterAnnotations (ann => !(strippedCPSAnns exists (ann matches _)))

  class MatchTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case Match(sel, cases) =>
        val origTp = tree.tpe
        // setType origTp intended for CPS -- TODO: is it necessary?
        val translated = translator.translateMatch(treeCopy.Match(tree, transform(sel), transformTrees(cases).asInstanceOf[List[CaseDef]]))
        try {
          localTyper.typed(translated) setType origTp
        } catch {
          case x: (Types#TypeError) =>
            // TODO: this should never happen; error should've been reported during type checking
            unit.error(tree.pos, "error during expansion of this match (this is a scalac bug).\nThe underlying error was: "+ x.msg)
            translated
        }
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
       }

   * P and M are derived from one's signature (`def one[T](x: P[T]): M[T]`)


   * if no `__match` is found, we assume the following implementation (and generate optimized code accordingly)

       object __match extends MatchStrategy[({type Id[x] = x})#Id, Option] {
         def zero = None
         def one[T](x: T) = Some(x)
         // NOTE: guard's return type must be of the shape M[T], where M is the monad in which the pattern match should be interpreted
         def guard[T](cond: Boolean, then: => T): Option[T] = if(cond) Some(then) else None
         def runOrElse[T, U](x: T)(f: T => Option[U]): U = f(x) getOrElse (throw new MatchError(x))
       }

   */
  trait MatchMonadInterface {
    val typer: Typer
    val matchOwner = typer.context.owner

    def reportUnreachable(pos: Position) = typer.context.unit.warning(pos, "unreachable code")
    def reportMissingCases(pos: Position, counterExamples: List[String]) = {
      val ceString =
        if (counterExamples.tail.isEmpty) "input: " + counterExamples.head
        else "inputs: " + counterExamples.mkString(", ")

      typer.context.unit.warning(pos, "match may not be exhaustive.\nIt would fail on the following "+ ceString)
    }

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
    def checkMatchVariablePatterns(m: Match) {
      // A string describing the first variable pattern
      var vpat: String = null
      // Using an iterator so we can recognize the last case
      val it = m.cases.iterator

      def addendum(pat: Tree) = {
        matchingSymbolInScope(pat) match {
          case NoSymbol   => ""
          case sym        =>
            val desc = if (sym.isParameter) s"parameter ${sym.nameString} of" else sym + " in"
            s"\nIf you intended to match against $desc ${sym.owner}, you must use backticks, like: case `${sym.nameString}` =>"
        }
      }

      while (it.hasNext) {
        val cdef = it.next
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
      checkMatchVariablePatterns(match_)

      // we don't transform after uncurry
      // (that would require more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      if(phase.id >= currentRun.uncurryPhase.id) debugwarn("running translateMatch at "+ phase +" on "+ selector +" match "+ cases)
      patmatDebug("translating "+ cases.mkString("{", "\n", "}"))

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatNanos) else null

      val selectorTp = repeatedToSeq(elimAnonymousClass(selector.tpe.widen.withoutAnnotations))

      val origPt  = match_.tpe
      // when one of the internal cps-type-state annotations is present, strip all CPS annotations
      // a cps-type-state-annotated type makes no sense as an expected type (matchX.tpe is used as pt in translateMatch)
      // (only test availability of MarkerCPSAdaptPlus assuming they are either all available or none of them are)
      val ptUnCPS =
        if (MarkerCPSAdaptPlus != NoSymbol && (stripTriggerCPSAnns exists origPt.hasAnnotation))
          removeCPSAdaptAnnotations(origPt)
        else origPt

      // relevant test cases: pos/existentials-harmful.scala, pos/gadt-gilles.scala, pos/t2683.scala, pos/virtpatmat_exist4.scala
      // pt is the skolemized version
      val pt = repeatedToSeq(ptUnCPS)

      // val packedPt = repeatedToSeq(typer.packedType(match_, context.owner))

      // the alternative to attaching the default case override would be to simply
      // append the default to the list of cases and suppress the unreachable case error that may arise (once we detect that...)
      val matchFailGenOverride = match_.attachments.get[DefaultOverrideMatchAttachment].map{case DefaultOverrideMatchAttachment(default) => ((scrut: Tree) => default)}

      val selectorSym = freshSym(selector.pos, pureType(selectorTp)) setFlag treeInfo.SYNTH_CASE_FLAGS

      // pt = Any* occurs when compiling test/files/pos/annotDepMethType.scala  with -Xexperimental
      val combined = combineCases(selector, selectorSym, cases map translateCase(selectorSym, pt), pt, matchOwner, matchFailGenOverride)

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
      def withSubPats(treeMakers: List[TreeMaker], subpats: (Symbol, Tree)*): TranslationStep = (treeMakers, subpats.toList)
      def noFurtherSubPats(treeMakers: TreeMaker*): TranslationStep = (treeMakers.toList, Nil)

      val pos = patTree.pos

      def translateExtractorPattern(extractor: ExtractorCall): TranslationStep = {
        if (!extractor.isTyped) ErrorUtils.issueNormalTypeError(patTree, "Could not typecheck extractor call: "+ extractor)(context)
        // if (extractor.resultInMonad == ErrorType) throw new TypeError(pos, "Unsupported extractor type: "+ extractor.tpe)

        patmatDebug("translateExtractorPattern checking parameter type: "+ (patBinder, patBinder.info.widen, extractor.paramType, patBinder.info.widen <:< extractor.paramType))

        // must use type `tp`, which is provided by extractor's result, not the type expected by binder,
        // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
        // (it will later result in a type test when `tp` is not a subtype of `b.info`)
        // TODO: can we simplify this, together with the Bound case?
        (extractor.subPatBinders, extractor.subPatTypes).zipped foreach { case (b, tp) =>
          patmatDebug("changing "+ b +" : "+ b.info +" -> "+ tp)
          b setInfo tp
        }

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
          } else {
            // no type test needed, but the tree maker relies on `patBinderOrCasted` having type `extractor.paramType` (and not just some type compatible with it)
            // SI-6624 shows this is necessary because apparently patBinder may have an unfortunate type (.decls don't have the case field accessors)
            // TODO: get to the bottom of this -- I assume it happens when type checking infers a weird type for an unapply call
            // by going back to the parameterType for the extractor call we get a saner type, so let's just do that for now
            /* TODO: uncomment when `settings.developer` and `devWarning` become available
              if (settings.developer.value && !(patBinder.info =:= extractor.paramType))
                devWarning(s"resetting info of $patBinder: ${patBinder.info} to ${extractor.paramType}")
            */
            (Nil, patBinder setInfo extractor.paramType)
          }

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
          val next = glb(List(patBinder.info.widen, pt)).normalize
          // a typed pattern never has any subtrees
          noFurtherSubPats(TypeTestTreeMaker(subPatBinder, patBinder, pt, next)(pos))

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
        case Literal(Constant(_)) | Ident(_) | Select(_, _) | This(_) =>
          noFurtherSubPats(EqualityTestTreeMaker(patBinder, patTree, pos))

        case Alternative(alts)    =>
          noFurtherSubPats(AlternativesTreeMaker(patBinder, alts map (translatePattern(patBinder, _)), alts.head.pos))

      /* TODO: Paul says about future version: I think this should work, and always intended to implement if I can get away with it.
          case class Foo(x: Int, y: String)
          case class Bar(z: Int)

          def f(x: Any) = x match { case Foo(x, _) | Bar(x) => x } // x is lub of course.
      */

        case Bind(n, p) => // this happens in certain ill-formed programs, there'll be an error later
          patmatDebug("WARNING: Bind tree with unbound symbol "+ patTree)
          noFurtherSubPats() // there's no symbol -- something's wrong... don't fail here though (or should we?)

        // case Star(_) | ArrayValue  => error("stone age pattern relics encountered!")

        case _                       =>
          typer.context.unit.error(patTree.pos, s"unsupported pattern: $patTree (a ${patTree.getClass}).\n This is a scalac bug. Tree diagnostics: ${asCompactDebugString(patTree)}.")
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
          val formalsWithRepeated = rawSubPatTypes.init :+ typeRef(pre, RepeatedParamClass, args)

          if (lastIsStar) formalTypes(formalsWithRepeated, nbSubPats - 1) :+ seqTp
          else formalTypes(formalsWithRepeated, nbSubPats)
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
        val paramAccessors = binder.constrParamAccessors
        // binders corresponding to mutable fields should be stored (SI-5158, SI-6070)
        val mutableBinders =
          if (paramAccessors exists (_.isMutable))
            subPatBinders.zipWithIndex.collect{ case (binder, idx) if paramAccessors(idx).isMutable => binder }
          else Nil

        // checks binder ne null before chaining to the next extractor
        ProductExtractorTreeMaker(binder, lengthGuard(binder))(subPatBinders, subPatRefs(binder), mutableBinders)
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
        ExtractorTreeMaker(extractorApply, lengthGuard(binder), binder)(subPatBinders, subPatRefs(binder), resultType.typeSymbol == BooleanClass, checkedLength, patBinderOrCasted)
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
              treeCopy.Apply(t, x, List(CODE.REF(binder).setPos(i.pos)))
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
          private def typedIfOrigTyped(to: Tree, origTp: Type): Tree =
            if (origTp == null || origTp == NoType) to
            // important: only type when actually substing and when original tree was typed
            // (don't need to use origTp as the expected type, though, and can't always do this anyway due to unknown type params stemming from polymorphic extractors)
            else typer.typed(to, EXPRmode, WildcardType)

          override def transform(tree: Tree): Tree = {
            def subst(from: List[Symbol], to: List[Tree]): Tree =
              if (from.isEmpty) tree
              else if (tree.symbol == from.head) typedIfOrigTyped(to.head.shallowDuplicate.setPos(tree.pos), tree.tpe)
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
          patmatDebug("BUG: incorporateOuterSubstitution called more than once for "+ (this, currSub, outerSubst))
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

    // unless we're optimizing, emit local variable bindings for all subpatterns of extractor/case class patterns
    protected val debugInfoEmitVars = !settings.optimise.value

    trait PreserveSubPatBinders extends TreeMaker {
      val subPatBinders: List[Symbol]
      val subPatRefs: List[Tree]

      // unless `debugInfoEmitVars`, this set should contain the bare minimum for correctness
      // mutable case class fields need to be stored regardless (SI-5158, SI-6070) -- see override in ProductExtractorTreeMaker
      def storedBinders: Set[Symbol] = if (debugInfoEmitVars) subPatBinders.toSet else Set.empty

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

      import CODE._
      def bindSubPats(in: Tree): Tree = if (!emitVars) in
        else {
          val (subPatBindersStored, subPatRefsStored) = stored.unzip
          Block(map2(subPatBindersStored.toList, subPatRefsStored.toList)(VAL(_) === _), in)
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
          extractorReturnsBoolean: Boolean,
          val checkedLength: Option[Int],
          val prevBinder: Symbol) extends FunTreeMaker with PreserveSubPatBinders {

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

      override def toString = "X"+(extractor, nextBinder.name)
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
          val mutableBinders: List[Symbol]) extends FunTreeMaker with PreserveSubPatBinders {

      import CODE._
      val nextBinder = prevBinder // just passing through

      // mutable binders must be stored to avoid unsoundness or seeing mutation of fields after matching (SI-5158, SI-6070)
      // (the implementation could be optimized by duplicating code from `super.storedBinders`, but this seems more elegant)
      override def storedBinders: Set[Symbol] = super.storedBinders ++ mutableBinders.toSet

      def chainBefore(next: Tree)(casegen: Casegen): Tree = {
        val nullCheck = REF(prevBinder) OBJ_NE NULL
        val cond = extraCond map (nullCheck AND _) getOrElse nullCheck
        casegen.ifThenElseZero(cond, bindSubPats(substitution(next)))
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
            case ThisType(clazz)      => THIS(clazz)
            case pre if pre != NoType => REF(pre.prefix, pre.termSymbol)
            case _ => TRUE_typed // fallback for SI-6183
          }

          // ExplicitOuter replaces `Select(q, outerSym) OBJ_EQ expectedPrefix` by `Select(q, outerAccessor(outerSym.owner)) OBJ_EQ expectedPrefix`
          // if there's an outer accessor, otherwise the condition becomes `true` -- TODO: can we improve needsOuterTest so there's always an outerAccessor?
          val outer = expectedTp.typeSymbol.newMethod(vpmName.outer, newFlags = SYNTHETIC | ARTIFACT) setInfo expectedTp.prefix

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
      patmatDebug("TTTM"+(prevBinder, extractorArgTypeTest, testedBinder, expectedTp, nextBinderTp))

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
        def expTp(t: Tree): t.type = t setType expectedTp

        // true when called to type-test the argument to an extractor
        // don't do any fancy equality checking, just test the type
        if (extractorArgTypeTest) default
        else expectedTp match {
          // TODO: [SPEC] the spec requires `eq` instead of `==` for singleton types
          // this implies sym.isStable
          case SingleType(_, sym)                       => and(equalsTest(gen.mkAttributedQualifier(expectedTp), testedBinder), typeTest(testedBinder, expectedTp.widen))
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

    // pt is the fully defined type of the cases (either pt or the lub of the types of the cases)
    def combineCasesNoSubstOnly(scrut: Tree, scrutSym: Symbol, casesNoSubstOnly: List[List[TreeMaker]], pt: Type, owner: Symbol, matchFailGenOverride: Option[Tree => Tree]): Tree =
      fixerUpper(owner, scrut.pos){
        def matchFailGen = (matchFailGenOverride orElse Some(CODE.MATCHERROR(_: Tree)))
        patmatDebug("combining cases: "+ (casesNoSubstOnly.map(_.mkString(" >> ")).mkString("{", "\n", "}")))

        val (unchecked, requireSwitch) =
          if (settings.XnoPatmatAnalysis.value) (true, false)
          else scrut match {
            case Typed(_, tpt) =>
              (tpt.tpe hasAnnotation UncheckedClass,
               // matches with two or fewer cases need not apply for switchiness (if-then-else will do)
               treeInfo.isSwitchAnnotation(tpt.tpe) && casesNoSubstOnly.lengthCompare(2) > 0)
            case _ =>
              (false, false)
          }

        emitSwitch(scrut, scrutSym, casesNoSubstOnly, pt, matchFailGenOverride, unchecked).getOrElse{
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
            patmatDebug("new symbol for "+ (t, t.symbol.ownerChain))
          case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == origOwner) =>
            patmatDebug("fundef: "+ (t, t.symbol.ownerChain, currentOwner.ownerChain))
            t.symbol.owner = currentOwner
          case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == origOwner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
            patmatDebug("def: "+ (d, d.symbol.ownerChain, currentOwner.ownerChain))
            if(d.symbol.moduleClass ne NoSymbol)
              d.symbol.moduleClass.owner = currentOwner

            d.symbol.owner = currentOwner
          // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
          patmatDebug("untouched "+ (t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain))
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
      NoSymbol.newTermSymbol(freshName(prefix), pos, newFlags = SYNTHETIC) setInfo tp

    def newSynthCaseLabel(name: String) =
      NoSymbol.newLabel(freshName(name), NoPosition) setFlag treeInfo.SYNTH_CASE_FLAGS

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree

      // local / context-free
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      def _asInstanceOf(t: Tree, tp: Type): Tree
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

    // we use subtyping as a model for implication between instanceof tests
    // i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
    // unfortunately this is not true in general:
    // SI-6022 expects instanceOfTpImplies(ProductClass.tpe, AnyRefClass.tpe)
    def instanceOfTpImplies(tp: Type, tpImplied: Type) = {
      val tpValue    = tp.typeSymbol.isPrimitiveValueClass

      // pretend we're comparing to Any when we're actually comparing to AnyVal or AnyRef
      // (and the subtype is respectively a value type or not a value type)
      // this allows us to reuse subtyping as a model for implication between instanceOf tests
      // the latter don't see a difference between AnyRef, Object or Any when comparing non-value types -- SI-6022
      val tpImpliedNormalizedToAny =
        if (tpImplied =:= (if (tpValue) AnyValClass.tpe else AnyRefClass.tpe)) AnyClass.tpe
        else tpImplied

      tp <:< tpImpliedNormalizedToAny
    }

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
      def _asInstanceOf(t: Tree, tp: Type): Tree = if (t.tpe != NoType && t.isTyped && typesConform(t.tpe, tp)) t else mkCast(t, tp)
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
      // private val reusedBy = new scala.collection.mutable.HashSet[Test]
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

    // TODO: remove Cond, replace by Prop from Logic
    object Cond {
      var currId = 0
    }

    abstract class Cond {
      val id = { Cond.currId += 1; Cond.currId}
    }

    case object TrueCond extends Cond  {override def toString = "T"}
    case object FalseCond extends Cond {override def toString = "F"}

    case class AndCond(a: Cond, b: Cond) extends Cond {override def toString = a +"/\\"+ b}
    case class OrCond(a: Cond, b: Cond)  extends Cond {override def toString = "("+a+") \\/ ("+ b +")"}

    object EqualityCond {
      private val uniques = new scala.collection.mutable.HashMap[(Tree, Tree), EqualityCond]
      def apply(testedPath: Tree, rhs: Tree): EqualityCond = uniques getOrElseUpdate((testedPath, rhs), new EqualityCond(testedPath, rhs))
      def unapply(c: EqualityCond) = Some((c.testedPath, c.rhs))
    }
    class EqualityCond(val testedPath: Tree, val rhs: Tree) extends Cond {
      override def toString = testedPath +" == "+ rhs +"#"+ id
    }

    object NonNullCond {
      private val uniques = new scala.collection.mutable.HashMap[Tree, NonNullCond]
      def apply(testedPath: Tree): NonNullCond = uniques getOrElseUpdate(testedPath, new NonNullCond(testedPath))
      def unapply(c: NonNullCond) = Some(c.testedPath)
    }
    class NonNullCond(val testedPath: Tree) extends Cond {
      override def toString = testedPath +" ne null " +"#"+ id
    }

    object TypeCond {
      private val uniques = new scala.collection.mutable.HashMap[(Tree, Type), TypeCond]
      def apply(testedPath: Tree, pt: Type): TypeCond = uniques getOrElseUpdate((testedPath, pt), new TypeCond(testedPath, pt))
      def unapply(c: TypeCond) = Some((c.testedPath, c.pt))
    }
    class TypeCond(val testedPath: Tree, val pt: Type) extends Cond {
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

    // TODO: improve, e.g., for constants
    def sameValue(a: Tree, b: Tree): Boolean = (a eq b) || ((a, b) match {
      case (_ : Ident, _ : Ident) => a.symbol eq b.symbol
      case _                      => false
    })

    object IrrefutableExtractorTreeMaker {
      // will an extractor with unapply method of methodtype `tp` always succeed?
      // note: this assumes the other side-conditions implied by the extractor are met
      // (argument of the right type, length check succeeds for unapplySeq,...)
      def irrefutableExtractorType(tp: Type): Boolean = tp.resultType.dealias match {
        case TypeRef(_, SomeClass, _) => true
        // probably not useful since this type won't be inferred nor can it be written down (yet)
        case ConstantType(Constant(true)) => true
        case _ => false
      }

      def unapply(xtm: ExtractorTreeMaker): Option[(Tree, Symbol)] = xtm match {
        case ExtractorTreeMaker(extractor, None, nextBinder) if irrefutableExtractorType(extractor.tpe) =>
          Some((extractor, nextBinder))
        case _ =>
          None
      }
    }

    // returns (tree, tests), where `tree` will be used to refer to `root` in `tests`
    class TreeMakersToConds(val root: Symbol) {
      // a variable in this set should never be replaced by a tree that "does not consist of a selection on a variable in this set" (intuitively)
      private val pointsToBound = scala.collection.mutable.HashSet(root)
      private val trees         = scala.collection.mutable.HashSet.empty[Tree]

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
        trees find (a => a.correspondsStructure(t)(sameValue)) match {
          case Some(orig) =>
            // patmatDebug("unique: "+ (t eq orig, orig))
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
        unique(accumSubst(normalize(CODE.REF(b))), b.tpe)

      def /\(conds: Iterable[Cond]) = if (conds.isEmpty) TrueCond else conds.reduceLeft(AndCond(_, _))
      def \/(conds: Iterable[Cond]) = if (conds.isEmpty) FalseCond else conds.reduceLeft(OrCond(_, _))

      // note that the sequencing of operations is important: must visit in same order as match execution
      // binderToUniqueTree uses the type of the first symbol that was encountered as the type for all future binders
      abstract class TreeMakerToCond extends (TreeMaker => Cond) {
        // requires(if (!substitutionComputed))
        def updateSubstitution(subst: Substitution): Unit = {
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
          // patmatDebug ("normalize subst: "+ normalize)

          val okSubst = Substitution(unboundFrom, unboundTo map (normalize(_))) // it's important substitution does not duplicate trees here -- it helps to keep hash consing simple, anyway
          pointsToBound ++= ((okSubst.from, okSubst.to).zipped filter { (f, t) => pointsToBound exists (sym => t.exists(_.symbol == sym)) })._1
          // patmatDebug("pointsToBound: "+ pointsToBound)

          accumSubst >>= okSubst
          // patmatDebug("accumSubst: "+ accumSubst)
        }

        def handleUnknown(tm: TreeMaker): Cond

        /** apply itself must render a faithful representation of the TreeMaker
         *
         * Concretely, TrueCond must only be used to represent a TreeMaker that is sure to match and that does not do any computation at all
         * e.g., doCSE relies on apply itself being sound in this sense (since it drops TreeMakers that are approximated to TrueCond -- SI-6077)
         *
         * handleUnknown may be customized by the caller to approximate further
         *
         * TODO: don't ignore outer-checks
         */
        def apply(tm: TreeMaker): Cond = {
          if (!substitutionComputed) updateSubstitution(tm.subPatternsAsSubstitution)

          tm match {
            case ttm@TypeTestTreeMaker(prevBinder, testedBinder, pt, _)   =>
              object condStrategy extends TypeTestTreeMaker.TypeTestCondStrategy {
                type Result                                           = Cond
                def and(a: Result, b: Result)                         = AndCond(a, b)
                def outerTest(testedBinder: Symbol, expectedTp: Type) = TrueCond // TODO OuterEqCond(testedBinder, expectedType)
                def typeTest(b: Symbol, pt: Type) = { // a type test implies the tested path is non-null (null.isInstanceOf[T] is false for all T)
                  val p = binderToUniqueTree(b);                        AndCond(NonNullCond(p), TypeCond(p, uniqueTp(pt)))
                }
                def nonNullTest(testedBinder: Symbol)                 = NonNullCond(binderToUniqueTree(testedBinder))
                def equalsTest(pat: Tree, testedBinder: Symbol)       = EqualityCond(binderToUniqueTree(testedBinder), unique(pat))
                def eqTest(pat: Tree, testedBinder: Symbol)           = EqualityCond(binderToUniqueTree(testedBinder), unique(pat)) // TODO: eq, not ==
              }
              ttm.renderCondition(condStrategy)
            case EqualityTestTreeMaker(prevBinder, patTree, _)        => EqualityCond(binderToUniqueTree(prevBinder), unique(patTree))
            case AlternativesTreeMaker(_, altss, _)                   => \/(altss map (alts => /\(alts map this)))
            case ProductExtractorTreeMaker(testedBinder, None)        => NonNullCond(binderToUniqueTree(testedBinder))
            case SubstOnlyTreeMaker(_, _)                             => TrueCond
            case GuardTreeMaker(guard) =>
              guard.tpe match {
                case ConstantType(Constant(true))  => TrueCond
                case ConstantType(Constant(false)) => FalseCond
                case _                             => handleUnknown(tm)
              }
            case ExtractorTreeMaker(_, _, _) |
                 ProductExtractorTreeMaker(_, _) |
                 BodyTreeMaker(_, _)               => handleUnknown(tm)
          }
        }
      }


      private val irrefutableExtractor: PartialFunction[TreeMaker, Cond] = {
        // the extra condition is None, the extractor's result indicates it always succeeds,
        // (the potential type-test for the argument is represented by a separate TypeTestTreeMaker)
        case IrrefutableExtractorTreeMaker(_, _) => TrueCond
      }

      // special-case: interpret pattern `List()` as `Nil`
      // TODO: make it more general List(1, 2) => 1 :: 2 :: Nil  -- not sure this is a good idea...
      private val rewriteListPattern: PartialFunction[TreeMaker, Cond] = {
        case p @ ExtractorTreeMaker(_, _, testedBinder)
          if testedBinder.tpe.typeSymbol == ListClass && p.checkedLength == Some(0) =>
            EqualityCond(binderToUniqueTree(p.prevBinder), unique(Ident(NilModule) setType NilModule.tpe))
      }
      val fullRewrite      = (irrefutableExtractor orElse rewriteListPattern)
      val refutableRewrite = irrefutableExtractor

      @inline def onUnknown(handler: TreeMaker => Cond) = new TreeMakerToCond {
        def handleUnknown(tm: TreeMaker) = handler(tm)
      }

      // used for CSE -- rewrite all unknowns to False (the most conserative option)
      object conservative extends TreeMakerToCond {
        def handleUnknown(tm: TreeMaker) = FalseCond
      }

      final def approximateMatch(cases: List[List[TreeMaker]], treeMakerToCond: TreeMakerToCond = conservative) ={
        val testss = cases.map { _ map (tm => Test(treeMakerToCond(tm), tm)) }
        substitutionComputed = true // a second call to approximateMatch should not re-compute the substitution (would be wrong)
        testss
      }
    }

    def approximateMatchConservative(root: Symbol, cases: List[List[TreeMaker]]): List[List[Test]] =
      (new TreeMakersToConds(root)).approximateMatch(cases)

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

    type Const

    type TypeConst <: Const
    def TypeConst: TypeConstExtractor
    trait TypeConstExtractor {
      def apply(tp: Type): Const
    }

    def NullConst: Const

    type Var <: AbsVar

    trait AbsVar {
      // indicate we may later require a prop for V = C
      def registerEquality(c: Const): Unit

      // call this to indicate null is part of the domain
      def registerNull(): Unit

      // can this variable be null?
      def mayBeNull: Boolean

      // compute the domain and return it (call registerNull first!)
      def domainSyms: Option[Set[Sym]]

      // the symbol for this variable being equal to its statically known type
      // (only available if registerEquality has been called for that type before)
      def symForStaticTp: Option[Sym]

      // for this var, call it V, turn V = C into the equivalent proposition in boolean logic
      // registerEquality(c) must have been called prior to this call
      // in fact, all equalities relevant to this variable must have been registered
      def propForEqualsTo(c: Const): Prop

      // populated by registerEquality
      // once implications has been called, must not call registerEquality anymore
      def implications: List[(Sym, List[Sym], List[Sym])]
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


    def /\(props: Iterable[Prop]) = if (props.isEmpty) True else props.reduceLeft(And(_, _))
    def \/(props: Iterable[Prop]) = if (props.isEmpty) False else props.reduceLeft(Or(_, _))


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
    def removeVarEq(props: List[Prop], modelNull: Boolean = false): (Prop, List[Prop]) = {
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaVarEq) else null

      val vars = new scala.collection.mutable.HashSet[Var]

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
      if (modelNull) vars foreach (_.registerNull)

      val pure = props map rewriteEqualsToProp.apply

      var eqAxioms: Prop = True
      def addAxiom(p: Prop) = eqAxioms = And(eqAxioms, p)

      patmatDebug("removeVarEq vars: "+ vars)
      vars.foreach { v =>
        // if v.domainSyms.isEmpty, we must consider the domain to be infinite
        // otherwise, since the domain fully partitions the type of the value,
        // exactly one of the types (and whatever it implies, imposed separately) must be chosen
        // consider X ::= A | B | C, and A => B
        // coverage is formulated as: A \/ B \/ C and the implications are
        v.domainSyms foreach { dsyms => addAxiom(\/(dsyms)) }

        // when this variable cannot be null the equality corresponding to the type test `(x: T)`, where T is x's static type,
        // is always true; when the variable may be null we use the implication `(x != null) => (x: T)` for the axiom
        v.symForStaticTp foreach { symForStaticTp =>
          if (v.mayBeNull) addAxiom(Or(v.propForEqualsTo(NullConst), symForStaticTp))
          else addAxiom(symForStaticTp)
        }

        v.implications foreach { case (sym, implied, excluded) =>
          // when sym is true, what must hold...
          implied  foreach (impliedSym  => addAxiom(Or(Not(sym), impliedSym)))
          // ... and what must not?
          excluded foreach (excludedSym => addAxiom(Or(Not(sym), Not(excludedSym))))
        }
      }

      patmatDebug("eqAxioms:\n"+ cnfString(eqFreePropToSolvable(eqAxioms)))
      patmatDebug("pure:"+ pure.map(p => cnfString(eqFreePropToSolvable(p))).mkString("\n"))

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaVarEq, start)

      (eqAxioms, pure)
    }


    type Formula
    def andFormula(a: Formula, b: Formula): Formula


    // may throw an AnalysisBudget.Exception
    def propToSolvable(p: Prop): Formula = {
      val (eqAxioms, pure :: Nil) = removeVarEq(List(p), modelNull = false)
      eqFreePropToSolvable(And(eqAxioms, pure))
    }

    // may throw an AnalysisBudget.Exception
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
    /** Override Array creation for efficiency (to not go through reflection). */
    private implicit val clauseTag: scala.reflect.ClassTag[Clause] = new scala.reflect.ClassTag[Clause] {
      def runtimeClass: java.lang.Class[Clause] = classOf[Clause]
      final override def newArray(len: Int): Array[Clause] = new Array[Clause](len)
    }
    def formula(c: Clause*): Formula = c.toArray
    def andFormula(a: Formula, b: Formula): Formula = a ++ b

    // a clause is a disjunction of distinct literals
    type Clause = Set[Lit]
    def clause(l: Lit*): Clause = l.toSet
    private def merge(a: Clause, b: Clause) = a ++ b

    type Lit
    def Lit(sym: Sym, pos: Boolean = true): Lit

    // throws an AnalysisBudget.Exception when the prop results in a CNF that's too big
    // TODO: be smarter/more efficient about this (http://lara.epfl.ch/w/sav09:tseitin_s_encoding)
    def eqFreePropToSolvable(p: Prop): Formula = {
      def negationNormalFormNot(p: Prop, budget: Int = AnalysisBudget.max): Prop =
        if (budget <= 0) throw AnalysisBudget.exceeded
        else p match {
          case And(a, b) =>  Or(negationNormalFormNot(a, budget - 1), negationNormalFormNot(b, budget - 1))
          case Or(a, b)  => And(negationNormalFormNot(a, budget - 1), negationNormalFormNot(b, budget - 1))
          case Not(p)    => negationNormalForm(p, budget - 1)
          case True      => False
          case False     => True
          case s: Sym    => Not(s)
        }

      def negationNormalForm(p: Prop, budget: Int = AnalysisBudget.max): Prop =
        if (budget <= 0) throw AnalysisBudget.exceeded
        else p match {
          case And(a, b)      => And(negationNormalForm(a, budget - 1), negationNormalForm(b, budget - 1))
          case Or(a, b)       =>  Or(negationNormalForm(a, budget - 1), negationNormalForm(b, budget - 1))
          case Not(negated)   => negationNormalFormNot(negated, budget - 1)
          case True
             | False
             | (_ : Sym)      => p
        }

      val TrueF          = formula()
      val FalseF         = formula(clause())
      def lit(s: Sym)    = formula(clause(Lit(s)))
      def negLit(s: Sym) = formula(clause(Lit(s, false)))

      def conjunctiveNormalForm(p: Prop, budget: Int = AnalysisBudget.max): Formula = {
        def distribute(a: Formula, b: Formula, budget: Int): Formula =
          if (budget <= 0) throw AnalysisBudget.exceeded
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

        if (budget <= 0) throw AnalysisBudget.exceeded

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

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatCNF) else null
      val res   = conjunctiveNormalForm(negationNormalForm(p))

      if (Statistics.canEnable) Statistics.stopTimer(patmatCNF, start)

      //
      if (Statistics.canEnable) patmatCNFSizes(res.size).value += 1

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
          patmatDebug("find all models for\n"+ cnfString(f))
          val model = findModelFor(f)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoModel) {
            val unassigned = (vars -- model.keySet).toList
            patmatDebug("unassigned "+ unassigned +" in "+ model)
            def force(lit: Lit) = {
              val model = withLit(findModelFor(dropUnit(f, lit)), lit)
              if (model ne NoModel) List(model)
              else Nil
            }
            val forced = unassigned flatMap { s =>
              force(Lit(s, true)) ++ force(Lit(s, false))
            }
            patmatDebug("forced "+ forced)
            val negated = negateModel(model)
            findAllModels(f :+ negated, model :: (forced ++ models), recursionDepthAllowed - 1)
          }
          else models
        }

      findAllModels(f, Nil)
    }

    private def withLit(res: Model, l: Lit): Model = if (res eq NoModel) NoModel else res + (l.sym -> l.pos)
    private def dropUnit(f: Formula, unitLit: Lit) = {
      val negated = -unitLit
      // drop entire clauses that are trivially true
      // (i.e., disjunctions that contain the literal we're making true in the returned model),
      // and simplify clauses by dropping the negation of the literal we're making true
      // (since False \/ X == X)
      f.filterNot(_.contains(unitLit)).map(_ - negated)
    }

    def findModelFor(f: Formula): Model = {
      @inline def orElse(a: Model, b: => Model) = if (a ne NoModel) a else b

      patmatDebug("DPLL\n"+ cnfString(f))

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaDPLL) else null

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

        if (Statistics.canEnable) Statistics.stopTimer(patmatAnaDPLL, start)

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
      private val uniques = new scala.collection.mutable.HashMap[Tree, Var]
      def apply(x: Tree): Var = uniques getOrElseUpdate(x, new Var(x, x.tpe))
    }
    class Var(val path: Tree, staticTp: Type) extends AbsVar {
      private[this] val id: Int = Var.nextId

      // private[this] var canModify: Option[Array[StackTraceElement]] = None
      private[this] def ensureCanModify = {} //if (canModify.nonEmpty) patmatDebug("BUG!"+ this +" modified after having been observed: "+ canModify.get.mkString("\n"))

      private[this] def observed = {} //canModify = Some(Thread.currentThread.getStackTrace)

      // don't access until all potential equalities have been registered using registerEquality
      private[this] val symForEqualsTo = new scala.collection.mutable.HashMap[Const, Sym]

      // when looking at the domain, we only care about types we can check at run time
      val staticTpCheckable: Type = checkableType(staticTp)

      private[this] var _mayBeNull = false
      def registerNull(): Unit = { ensureCanModify; if (NullTp <:< staticTpCheckable) _mayBeNull = true }
      def mayBeNull: Boolean = _mayBeNull

      // case None => domain is unknown,
      // case Some(List(tps: _*)) => domain is exactly tps
      // we enumerate the subtypes of the full type, as that allows us to filter out more types statically,
      // once we go to run-time checks (on Const's), convert them to checkable types
      // TODO: there seems to be bug for singleton domains (variable does not show up in model)
      lazy val domain: Option[Set[Const]] = {
        val subConsts = enumerateSubtypes(staticTp).map{ tps =>
          tps.toSet[Type].map{ tp =>
            val domainC = TypeConst(tp)
            registerEquality(domainC)
            domainC
          }
        }

        val allConsts =
          if (mayBeNull) {
            registerEquality(NullConst)
            subConsts map (_ + NullConst)
          } else
            subConsts

        observed; allConsts
      }

      // populate equalitySyms
      // don't care about the result, but want only one fresh symbol per distinct constant c
      def registerEquality(c: Const): Unit = {ensureCanModify; symForEqualsTo getOrElseUpdate(c, Sym(this, c))}

      // return the symbol that represents this variable being equal to the constant `c`, if it exists, otherwise False (for robustness)
      // (registerEquality(c) must have been called prior, either when constructing the domain or from outside)
      def propForEqualsTo(c: Const): Prop = {observed; symForEqualsTo.getOrElse(c, False)}

      // [implementation NOTE: don't access until all potential equalities have been registered using registerEquality]p
      /** the information needed to construct the boolean proposition that encods the equality proposition (V = C)
       *
       * that models a type test pattern `_: C` or constant pattern `C`, where the type test gives rise to a TypeConst C,
       * and the constant pattern yields a ValueConst C
       *
       * for exhaustivity, we really only need implication (e.g., V = 1 implies that V = 1 /\ V = Int, if both tests occur in the match,
       * and thus in this variable's equality symbols), but reachability also requires us to model things like V = 1 precluding V = "1"
       */
      lazy val implications = {
        /** when we know V = C, which other equalities must hold
         *
         * in general, equality to some type implies equality to its supertypes
         * (this multi-valued kind of equality is necessary for unreachability)
         * note that we use subtyping as a model for implication between instanceof tests
         * i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
         * unfortunately this is not true in general (see e.g. SI-6022)
         */
        def implies(lower: Const, upper: Const): Boolean =
          // values and null
            lower == upper ||
          // type implication
            (lower != NullConst && !upper.isValue &&
             instanceOfTpImplies(if (lower.isValue) lower.wideTp else lower.tp, upper.tp))

          // if(r) patmatDebug("implies    : "+(lower, lower.tp, upper, upper.tp))
          // else  patmatDebug("NOT implies: "+(lower, upper))


        /** does V = C preclude V having value `other`?
         (1) V = null is an exclusive assignment,
         (2) V = A and V = B, for A and B value constants, are mutually exclusive unless A == B
             we err on the safe side, for example:
               - assume `val X = 1; val Y = 1`, then
                 (2: Int) match { case X => case Y =>  <falsely considered reachable>  }
               - V = 1 does not preclude V = Int, or V = Any, it could be said to preclude V = String, but we don't model that

         (3) for types we could try to do something fancy, but be conservative and just say no
         */
        def excludes(a: Const, b: Const): Boolean =
          a != b && ((a == NullConst || b == NullConst) || (a.isValue && b.isValue))

          // if(r) patmatDebug("excludes    : "+(a, a.tp, b, b.tp))
          // else  patmatDebug("NOT excludes: "+(a, b))

/*
[ HALF BAKED FANCINESS: //!equalitySyms.exists(common => implies(common.const, a) && implies(common.const, b)))
 when type tests are involved, we reason (conservatively) under a closed world assumption,
 since we are really only trying to counter the effects of the symbols that we introduce to model type tests
 we don't aim to model the whole subtyping hierarchy, simply to encode enough about subtyping to do unreachability properly

 consider the following hierarchy:

    trait A
    trait B
    trait C
    trait AB extends B with A

  // two types are mutually exclusive if there is no equality symbol whose constant implies both
  object Test extends App {
    def foo(x: Any) = x match {
      case _ : C  => println("C")
      case _ : AB => println("AB")
      case _ : (A with B) => println("AB'")
      case _ : B  => println("B")
      case _ : A  => println("A")
    }

 of course this kind of reasoning is not true in general,
 but we can safely pretend types are mutually exclusive as long as there are no counter-examples in the match we're analyzing}
*/

        val excludedPair = new scala.collection.mutable.HashSet[ExcludedPair]

        case class ExcludedPair(a: Const, b: Const) {
          override def equals(o: Any) = o match {
            case ExcludedPair(aa, bb) => (a == aa && b == bb) || (a == bb && b == aa)
            case _ => false
          }
          // make ExcludedPair(a, b).hashCode == ExcludedPair(b, a).hashCode
          override def hashCode = a.hashCode ^ b.hashCode
        }

        equalitySyms map { sym =>
          // if we've already excluded the pair at some point (-A \/ -B), then don't exclude the symmetric one (-B \/ -A)
          // (nor the positive implications -B \/ A, or -A \/ B, which would entail the equality axioms falsifying the whole formula)
          val todo = equalitySyms filterNot (b => (b.const == sym.const) || excludedPair(ExcludedPair(b.const, sym.const)))
          val (excluded, notExcluded) = todo partition (b => excludes(sym.const, b.const))
          val implied = notExcluded filter (b => implies(sym.const, b.const))

          patmatDebug("eq axioms for: "+ sym.const)
          patmatDebug("excluded: "+ excluded)
          patmatDebug("implied: "+ implied)

          excluded foreach { excludedSym => excludedPair += ExcludedPair(sym.const, excludedSym.const)}

          (sym, implied, excluded)
        }
      }

      // accessing after calling registerNull will result in inconsistencies
      lazy val domainSyms: Option[Set[Sym]] = domain map { _ map symForEqualsTo }

      lazy val symForStaticTp: Option[Sym]  = symForEqualsTo.get(TypeConst(staticTpCheckable))

      // don't access until all potential equalities have been registered using registerEquality
      private lazy val equalitySyms = {observed; symForEqualsTo.values.toList}

      // don't call until all equalities have been registered and registerNull has been called (if needed)
      def describe = toString + ": " + staticTp + domain.map(_.mkString(" ::= ", " | ", "// "+ symForEqualsTo.keys)).getOrElse(symForEqualsTo.keys.mkString(" ::= ", " | ", " | ...")) + " // = " + path
      override def toString = "V"+ id
    }


    // all our variables range over types
    // a literal constant becomes ConstantType(Constant(v)) when the type allows it (roughly, anyval + string + null)
    // equality between variables: SingleType(x) (note that pattern variables cannot relate to each other -- it's always patternVar == nonPatternVar)
    object Const {
      def resetUniques() = {_nextTypeId = 0; _nextValueId = 0; uniques.clear() ; trees.clear()}

      private var _nextTypeId = 0
      def nextTypeId = {_nextTypeId += 1; _nextTypeId}

      private var _nextValueId = 0
      def nextValueId = {_nextValueId += 1; _nextValueId}

      private val uniques = new scala.collection.mutable.HashMap[Type, Const]
      private[SymbolicMatchAnalysis] def unique(tp: Type, mkFresh: => Const): Const =
        uniques.get(tp).getOrElse(
          uniques.find {case (oldTp, oldC) => oldTp =:= tp} match {
            case Some((_, c)) =>
              patmatDebug("unique const: "+ (tp, c))
              c
            case _ =>
              val fresh = mkFresh
              patmatDebug("uniqued const: "+ (tp, fresh))
              uniques(tp) = fresh
              fresh
          })

      private val trees = scala.collection.mutable.HashSet.empty[Tree]

      // hashconsing trees (modulo value-equality)
      private[SymbolicMatchAnalysis] def uniqueTpForTree(t: Tree): Type =
        // a new type for every unstable symbol -- only stable value are uniqued
        // technically, an unreachable value may change between cases
        // thus, the failure of a case that matches on a mutable value does not exclude the next case succeeding
        // (and thuuuuus, the latter case must be considered reachable)
        if (!t.symbol.isStable) t.tpe.narrow
        else trees find (a => a.correspondsStructure(t)(sameValue)) match {
          case Some(orig) =>
            patmatDebug("unique tp for tree: "+ (orig, orig.tpe))
            orig.tpe
          case _ =>
            // duplicate, don't mutate old tree (TODO: use a map tree -> type instead?)
            val treeWithNarrowedType = t.duplicate setType t.tpe.narrow
            patmatDebug("uniqued: "+ (t, t.tpe, treeWithNarrowedType.tpe))
            trees += treeWithNarrowedType
            treeWithNarrowedType.tpe
        }
    }

    sealed abstract class Const {
      def tp: Type
      def wideTp: Type

      def isAny = wideTp.typeSymbol == AnyClass
      def isValue: Boolean //= tp.isStable

      // note: use reference equality on Const since they're hash-consed (doing type equality all the time is too expensive)
      // the equals inherited from AnyRef does just this
    }

    // find most precise super-type of tp that is a class
    // we skip non-class types (singleton types, abstract types) so that we can
    // correctly compute how types relate in terms of the values they rule out
    // e.g., when we know some value must be of type T, can it still be of type S? (this is the positive formulation of what `excludes` on Const computes)
    // since we're talking values, there must have been a class involved in creating it, so rephrase our types in terms of classes
    // (At least conceptually: `true` is an instance of class `Boolean`)
    private def widenToClass(tp: Type): Type =
      if (tp.typeSymbol.isClass) tp
      else tp.baseType(tp.baseClasses.head)

    object TypeConst extends TypeConstExtractor {
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

      val wideTp = widenToClass(tp)
      def isValue = false
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
          val wideTp = widenToClass(tp)

          val narrowTp =
            if (tp.isInstanceOf[SingletonType]) tp
            else p match {
              case Literal(c) =>
                if (c.tpe.typeSymbol == UnitClass) c.tpe
                else ConstantType(c)
              case Ident(_) if p.symbol.isStable =>
                // for Idents, can encode uniqueness of symbol as uniqueness of the corresponding singleton type
                // for Selects, which are handled by the next case, the prefix of the select varies independently of the symbol (see pos/virtpatmat_unreach_select.scala)
                singleType(tp.prefix, p.symbol)
              case _ =>
                Const.uniqueTpForTree(p)
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
      assert(!(tp =:= NullTp)) // TODO: assert(!tp.isStable)
      private[this] val id: Int = Const.nextValueId
      def isValue = true
    }

    lazy val NullTp = ConstantType(Constant(null))
    case object NullConst extends Const {
      def tp     = NullTp
      def wideTp = NullTp

      def isValue = true
      override def toString = "null"
    }


    // turns a case (represented as a list of abstract tests)
    // into a proposition that is satisfiable if the case may match
    def symbolicCase(tests: List[Test], modelNull: Boolean = false): Prop = {
      def symbolic(t: Cond): Prop = t match {
        case AndCond(a, b) => And(symbolic(a), symbolic(b))
        case OrCond(a, b) => Or(symbolic(a), symbolic(b))
        case TrueCond => True
        case FalseCond => False
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
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaReach) else null

      // use the same approximator so we share variables,
      // but need different conditions depending on whether we're conservatively looking for failure or success
      // don't rewrite List-like patterns, as List() and Nil need to distinguished for unreachability
      val approx = new TreeMakersToConds(prevBinder)
      def approximate(default: Cond) = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.refutableRewrite.applyOrElse(tm, (_: TreeMaker) => default )
      })

      val testCasesOk   = approximate(TrueCond)
      val testCasesFail = approximate(FalseCond)

      prepareNewAnalysis()

      val propsCasesOk   = testCasesOk   map (t => symbolicCase(t, modelNull = true))
      val propsCasesFail = testCasesFail map (t => Not(symbolicCase(t, modelNull = true)))
      val (eqAxiomsFail, symbolicCasesFail) = removeVarEq(propsCasesFail, modelNull = true)
      val (eqAxiomsOk, symbolicCasesOk)     = removeVarEq(propsCasesOk,   modelNull = true)

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

        patmatDebug("reachability, vars:\n"+ ((propsCasesFail flatMap gatherVariables).distinct map (_.describe) mkString ("\n")))
        patmatDebug("equality axioms:\n"+ cnfString(eqAxiomsCNF))

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

            // patmatDebug("trying to reach:\n"+ cnfString(eqFreePropToSolvable(current.head)) +"\nunder prefix:\n"+ cnfString(prefix))
            // if (NoModel ne model) patmatDebug("reached: "+ modelString(model))

            reachable = NoModel ne model
          }
        }

        if (Statistics.canEnable) Statistics.stopTimer(patmatAnaReach, start)

        if (reachable) None else Some(caseIndex)
      } catch {
        case ex: AnalysisBudget.Exception =>
          ex.warn(prevBinder.pos, "unreachability")
          None // CNF budget exceeded
      }
    }

    // exhaustivity

    // TODO: domain of other feasibly enumerable built-in types (char?)
    def enumerateSubtypes(tp: Type): Option[List[Type]] =
      tp.typeSymbol match {
        // TODO case _ if tp.isTupleType => // recurse into component types?
        case UnitClass =>
          Some(List(UnitClass.tpe))
        case BooleanClass =>
          Some((List(ConstantType(Constant(true)), ConstantType(Constant(false)))))
        // TODO case _ if tp.isTupleType => // recurse into component types
        case modSym: ModuleClassSymbol =>
          Some(List(tp))
        // make sure it's not a primitive, else (5: Byte) match { case 5 => ... } sees no Byte
        case sym if !sym.isSealed || isPrimitiveValueClass(sym) =>
          patmatDebug("enum unsealed "+ (tp, sym, sym.isSealed, isPrimitiveValueClass(sym)))
          None
        case sym =>
          val subclasses = (
            sym.sealedDescendants.toList sortBy (_.sealedSortName)
            // symbols which are both sealed and abstract need not be covered themselves, because
            // all of their children must be and they cannot otherwise be created.
            filterNot (x => x.isSealed && x.isAbstractClass && !isPrimitiveValueClass(x)))
          patmatDebug("enum sealed -- subclasses: "+ (sym, subclasses))

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
          patmatDebug("enum sealed "+ (tp, tpApprox) + " as "+ validSubTypes)
          Some(validSubTypes)
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
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) if args.nonEmpty && (sym ne ArrayClass) =>
            TypeRef(pre, sym, args map (_ => WildcardType))
          case _ =>
            mapOver(tp)
        }
      }

      val res = typeArgsToWildcardsExceptArray(tp)
      patmatDebug("checkable "+(tp, res))
      res
    }

    // a type is "uncheckable" (for exhaustivity) if we don't statically know its subtypes (i.e., it's unsealed)
    // we consider tuple types with at least one component of a checkable type as a checkable type
    def uncheckableType(tp: Type): Boolean = {
      def tupleComponents(tp: Type) = tp.normalize.typeArgs
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
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaExhaust) else null
      var backoff = false

      val approx = new TreeMakersToConds(prevBinder)
      val tests = approx.approximateMatch(cases, approx.onUnknown { tm =>
        approx.fullRewrite.applyOrElse[TreeMaker, Cond](tm, {
          case BodyTreeMaker(_, _) => TrueCond // irrelevant -- will be discarded by symbolCase later
          case _ => // patmatDebug("backing off due to "+ tm)
            backoff = true
            FalseCond
        })
      })

      if (backoff) Nil else {
        val prevBinderTree = approx.binderToUniqueTree(prevBinder)

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
        val vars = gatherVariables(matchFails)

  // debug output:
        patmatDebug("analysing:")
        showTreeMakers(cases)
        showTests(tests)

        // patmatDebug("\nvars:\n"+ (vars map (_.describe) mkString ("\n")))
        // patmatDebug("\nmatchFails as CNF:\n"+ cnfString(propToSolvable(matchFails)))

        try {
          // find the models (under which the match fails)
          val matchFailModels = findAllModelsFor(propToSolvable(matchFails))

          val scrutVar = Var(prevBinderTree)
          val counterExamples = matchFailModels.map(modelToCounterExample(scrutVar))

          val pruned = CounterExample.prune(counterExamples).map(_.toString).sorted

          if (Statistics.canEnable) Statistics.stopTimer(patmatAnaExhaust, start)
          pruned
        } catch {
          case ex : AnalysisBudget.Exception =>
            ex.warn(prevBinder.pos, "exhaustivity")
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

    def modelToVarAssignment(model: Model): Map[Var, (Seq[Const], Seq[Const])] =
      model.toSeq.groupBy{f => f match {case (sym, value) => sym.variable} }.mapValues{ xs =>
        val (trues, falses) = xs.partition(_._2)
        (trues map (_._1.const), falses map (_._1.const))
        // should never be more than one value in trues...
      }

    def varAssignmentString(varAssignment: Map[Var, (Seq[Const], Seq[Const])]) =
      varAssignment.toSeq.sortBy(_._1.toString).map { case (v, (trues, falses)) =>
         val assignment = "== "+ (trues mkString("(", ", ", ")")) +"  != ("+ (falses mkString(", ")) +")"
         v +"(="+ v.path +": "+ v.staticTpCheckable +") "+ assignment
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

      patmatDebug("var assignment for model "+ model +":\n"+ varAssignmentString(varAssignment))

      // chop a path into a list of symbols
      def chop(path: Tree): List[Symbol] = path match {
        case Ident(_) => List(path.symbol)
        case Select(pre, name) => chop(pre) :+ path.symbol
        case _ =>
          // patmatDebug("don't know how to chop "+ path)
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

        private val uniques = new scala.collection.mutable.HashMap[Var, VariableAssignment]
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
      case class VariableAssignment(variable: Var, equalTo: List[Const], notEqualTo: List[Const], fields: scala.collection.mutable.Map[Symbol, VariableAssignment]) {
        // need to prune since the model now incorporates all super types of a constant (needed for reachability)
        private lazy val uniqueEqualTo = equalTo filterNot (subsumed => equalTo.exists(better => (better ne subsumed) && instanceOfTpImplies(better.tp, subsumed.tp)))
        private lazy val prunedEqualTo = uniqueEqualTo filterNot (subsumed => variable.staticTpCheckable <:< subsumed.tp)
        private lazy val ctor       = (prunedEqualTo match { case List(TypeConst(tp)) => tp case _ => variable.staticTpCheckable }).typeSymbol.primaryConstructor
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
            patmatDebug("describing "+ (variable, equalTo, notEqualTo, fields, cls, allFieldAssignmentsLegal))
            val res = prunedEqualTo match {
              // a definite assignment to a value
              case List(eq: ValueConst) if fields.isEmpty => ValueExample(eq)

              // constructor call
              // or we did not gather any information about equality but we have information about the fields
              //  --> typical example is when the scrutinee is a tuple and all the cases first unwrap that tuple and only then test something interesting
              case _ if cls != NoSymbol && !isPrimitiveValueClass(cls) &&
                        (  uniqueEqualTo.nonEmpty
                        || (fields.nonEmpty && prunedEqualTo.isEmpty && notEqualTo.isEmpty)) =>

                def args(brevity: Boolean = beBrief) = {
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
                if (beBrief) WildcardExample
                else {
                  val eqTo = equalTo.headOption getOrElse TypeConst(variable.staticTpCheckable)
                  NegativeExample(eqTo, nonTrivialNonEqualTo)
                }

              // not a valid counter-example, possibly since we have a definite type but there was a field mismatch
              // TODO: improve reasoning -- in the mean time, a false negative is better than an annoying false positive
              case _ => NoExample
            }
            patmatDebug("described as: "+ res)
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
      patmatDebug("before CSE:")
      showTreeMakers(cases)

      val testss = approximateMatchConservative(prevBinder, cases)

      // interpret:
      val dependencies = new scala.collection.mutable.LinkedHashMap[Test, Set[Cond]]
      val tested = new scala.collection.mutable.HashSet[Cond]

      def storeDependencies(test: Test) = {
        val cond = test.cond

        def simplify(c: Cond): Set[Cond] = c match {
          case AndCond(a, b) => simplify(a) ++ simplify(b)
          case OrCond(_, _)   => Set(FalseCond) // TODO: make more precise
          case NonNullCond(_) => Set(TrueCond)  // not worth remembering
          case _ => Set(c)
        }
        val conds = simplify(cond)

        if (conds(FalseCond)) false // stop when we encounter a definite "no" or a "not sure"
        else {
          val nonTrivial = conds filterNot (_ == TrueCond)
          if (nonTrivial nonEmpty) {
            tested ++= nonTrivial

            // is there an earlier test that checks our condition and whose dependencies are implied by ours?
            dependencies find {
              case (priorTest, deps) =>
                ((simplify(priorTest.cond) == nonTrivial) || // our conditions are implied by priorTest if it checks the same thing directly
                 (nonTrivial subsetOf deps)                  // or if it depends on a superset of our conditions
                ) && (deps subsetOf tested)                 // the conditions we've tested when we are here in the match satisfy the prior test, and hence what it tested
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
      patmatDebug("dependencies: "+ dependencies)

      // find longest prefix of tests that reuse a prior test, and whose dependent conditions monotonically increase
      // then, collapse these contiguous sequences of reusing tests
      // store the result of the final test and the intermediate results in hoisted mutable variables (TODO: optimize: don't store intermediate results that aren't used)
      // replace each reference to a variable originally bound by a collapsed test by a reference to the hoisted variable
      val reused = new scala.collection.mutable.HashMap[TreeMaker, ReusedCondTreeMaker]
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
          (test.cond == TrueCond) || (for(
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

            patmatDebug("sharedPrefix: "+ sharedPrefix)
            patmatDebug("suffix: "+ sharedPrefix)
            // if the shared prefix contains interesting conditions (!= TrueCond)
            // and the last of such interesting shared conditions reuses another treemaker's test
            // replace the whole sharedPrefix by a ReusingCondTreeMaker
            for (lastShared <- sharedPrefix.reverse.dropWhile(_.cond == TrueCond).headOption;
                 lastReused <- lastShared.reuses)
              yield ReusingCondTreeMaker(sharedPrefix, reusedOrOrig) :: suffix.map(_.treeMaker)
          }

        collapsedTreeMakers getOrElse tests.map(_.treeMaker) // sharedPrefix need not be empty (but it only contains TrueCond-tests, which are dropped above)
      }
      okToCall = true // TODO: remove (debugging)

      // replace original treemakers that are reused (as determined when computing collapsed),
      // by ReusedCondTreeMakers
      val reusedMakers = collapsed mapConserve (_ mapConserve reusedOrOrig)
      patmatDebug("after CSE:")
      showTreeMakers(reusedMakers)
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
    import treeInfo.isGuardedCase

    abstract class SwitchMaker {
      abstract class SwitchableTreeMakerExtractor { def unapply(x: TreeMaker): Option[Tree] }
      val SwitchableTreeMaker: SwitchableTreeMakerExtractor

      def alternativesSupported: Boolean

      // when collapsing guarded switch cases we may sometimes need to jump to the default case
      // however, that's not supported in exception handlers, so when we can't jump when we need it, don't emit a switch
      // TODO: make more fine-grained, as we don't always need to jump
      def canJump: Boolean

      def unchecked: Boolean


      def isDefault(x: CaseDef): Boolean
      def defaultSym: Symbol
      def defaultBody: Tree
      def defaultCase(scrutSym: Symbol = defaultSym, guard: Tree = EmptyTree, body: Tree = defaultBody): CaseDef

      private def sequence[T](xs: List[Option[T]]): Option[List[T]] =
        if (xs exists (_.isEmpty)) None else Some(xs.flatten)

      object GuardAndBodyTreeMakers {
          def unapply(tms: List[TreeMaker]): Option[(Tree, Tree)] = {
            tms match {
              case (btm@BodyTreeMaker(body, _)) :: Nil => Some((EmptyTree, btm.substitution(body)))
              case (gtm@GuardTreeMaker(guard)) :: (btm@BodyTreeMaker(body, _)) :: Nil => Some((gtm.substitution(guard), btm.substitution(body)))
              case _ => None
            }
          }
      }

      private val defaultLabel: Symbol =  newSynthCaseLabel("default")

      /** Collapse guarded cases that switch on the same constant (the last case may be unguarded).
       *
       * Cases with patterns A and B switch on the same constant iff for all values x that match A also match B and vice versa.
       * (This roughly corresponds to equality on trees modulo alpha renaming and reordering of alternatives.)
       *
       * The rewrite only applies if some of the cases are guarded (this must be checked before invoking this method).
       *
       * The rewrite goes through the switch top-down and merges each case with the subsequent cases it is implied by
       * (i.e. it matches if they match, not taking guards into account)
       *
       * If there are no unreachable cases, all cases can be uniquely assigned to a partition of such 'overlapping' cases,
       * save for the default case (thus we jump to it rather than copying it several times).
       * (The cases in a partition are implied by the principal element of the partition.)
       *
       * The overlapping cases are merged into one case with their guards pushed into the body as follows
       * (with P the principal element of the overlapping patterns Pi):
       *
       *    `{case Pi if(G_i) => B_i }*` is rewritten to `case P => {if(G_i) B_i}*`
       *
       * The rewrite fails (and returns Nil) when:
       *   (1) there is a subsequence of overlapping cases that has an unguarded case in the middle;
       *       only the last case of each subsequence of overlapping cases may be unguarded (this is implied by unreachability)
       *
       *   (2) there are overlapping cases that differ (tested by `caseImpliedBy`)
       *       cases with patterns A and B are overlapping if for SOME value x, A matches x implies B matches y OR vice versa  <-- note the difference with case equality defined above
       *       for example `case 'a' | 'b' =>` and `case 'b' =>` are different and overlapping (overlapping and equality disregard guards)
       *
       * The second component of the returned tuple indicates whether we'll need to emit a labeldef to jump to the default case.
       */
      private def collapseGuardedCases(cases: List[CaseDef]): (List[CaseDef], Boolean) = {
        // requires(same.forall(caseEquals(same.head)))
        // requires(same.nonEmpty, same)
        def collapse(same: List[CaseDef], isDefault: Boolean): CaseDef = {
          val commonPattern = same.head.pat
          // jump to default case (either the user-supplied one or the synthetic one)
          // unless we're collapsing the default case: then we re-use the same body as the synthetic catchall (throwing a matcherror, rethrowing the exception)
          val jumpToDefault: Tree =
            if (isDefault || !canJump) defaultBody
            else Apply(Ident(defaultLabel), Nil)

          val guardedBody = same.foldRight(jumpToDefault){
            // the last case may be un-guarded (we know it's the last one since fold's accum == jumpToDefault)
            // --> replace jumpToDefault by the un-guarded case's body
            case (CaseDef(_, EmptyTree, b), `jumpToDefault`)     => b
            case (cd@CaseDef(_, g, b), els) if isGuardedCase(cd) => If(g, b, els)
          }

          // if the cases that we're going to collapse bind variables,
          // must replace them by the single binder introduced by the collapsed case
          val binders = same.collect{case CaseDef(x@Bind(_, _), _, _) if x.symbol != NoSymbol => x.symbol}
          val (pat, guardedBodySubst) =
            if (binders.isEmpty) (commonPattern, guardedBody)
            else {
              // create a single fresh binder to subsume the old binders (and their types)
              // TODO: I don't think the binder's types can actually be different (due to checks in caseEquals)
              // if they do somehow manage to diverge, the lub might not be precise enough and we could get a type error
              // TODO: reuse name exactly if there's only one binder in binders
              val binder = freshSym(binders.head.pos, lub(binders.map(_.tpe)), binders.head.name.toString)

              // the patterns in same are equal (according to caseEquals)
              // we can thus safely pick the first one arbitrarily, provided we correct binding
              val origPatWithoutBind = commonPattern match {
                case Bind(b, orig) => orig
                case o => o
              }
              // need to replace `defaultSym` as well -- it's used in `defaultBody` (see `jumpToDefault` above)
              val unifiedBody = guardedBody.substituteSymbols(defaultSym :: binders, binder :: binders.map(_ => binder))
              (Bind(binder, origPatWithoutBind), unifiedBody)
            }

          atPos(commonPattern.pos)(CaseDef(pat, EmptyTree, guardedBodySubst))
        }

        // requires cases.exists(isGuardedCase) (otherwise the rewrite is pointless)
        var remainingCases = cases
        val collapsed      = scala.collection.mutable.ListBuffer.empty[CaseDef]

        // when some of collapsed cases (except for the default case itself) did not include an un-guarded case
        // we'll need to emit a labeldef for the default case
        var needDefault  = false

        while (remainingCases.nonEmpty) {
          val currCase              = remainingCases.head
          val currIsDefault         = isDefault(CaseDef(currCase.pat, EmptyTree, EmptyTree))
          val (impliesCurr, others) =
            // the default case is implied by all cases, no need to partition (and remainingCases better all be default cases as well)
            if (currIsDefault) (remainingCases.tail, Nil)
            else remainingCases.tail partition (caseImplies(currCase))

          val unguardedComesLastOrAbsent =
            (!isGuardedCase(currCase) && impliesCurr.isEmpty) || { val LastImpliesCurr = impliesCurr.length - 1
            impliesCurr.indexWhere(oc => !isGuardedCase(oc)) match {
              // if all cases are guarded we will have to jump to the default case in the final else
              // (except if we're collapsing the default case itself)
              case -1 =>
                if (!currIsDefault) needDefault = true
                true

              // last case is not guarded, no need to jump to the default here
              // note: must come after case -1 => (since LastImpliesCurr may be -1)
              case LastImpliesCurr => true

              case _ => false
            }}

          if (unguardedComesLastOrAbsent /*(1)*/ && impliesCurr.forall(caseEquals(currCase)) /*(2)*/) {
            collapsed += (
              if (impliesCurr.isEmpty && !isGuardedCase(currCase)) currCase
              else collapse(currCase :: impliesCurr, currIsDefault)
            )

            remainingCases = others
          } else { // fail
            collapsed.clear()
            remainingCases = Nil
          }
        }

        (collapsed.toList, needDefault)
      }

      private def caseEquals(x: CaseDef)(y: CaseDef) = patternEquals(x.pat)(y.pat)
      private def patternEquals(x: Tree)(y: Tree): Boolean = (x, y) match {
        case (Alternative(xs), Alternative(ys)) =>
          xs.forall(x => ys.exists(patternEquals(x))) &&
          ys.forall(y => xs.exists(patternEquals(y)))
        case (Alternative(pats), _) => pats.forall(p => patternEquals(p)(y))
        case (_, Alternative(pats)) => pats.forall(q => patternEquals(x)(q))
        // regular switch
        case (Literal(Constant(cx)), Literal(Constant(cy))) => cx == cy
        case (Ident(nme.WILDCARD), Ident(nme.WILDCARD))     => true
        // type-switch for catch
        case (Bind(_, Typed(Ident(nme.WILDCARD), tpX)), Bind(_, Typed(Ident(nme.WILDCARD), tpY))) => tpX.tpe =:= tpY.tpe
        case _ => false
      }

      // if y matches then x matches for sure (thus, if x comes before y, y is unreachable)
      private def caseImplies(x: CaseDef)(y: CaseDef) = patternImplies(x.pat)(y.pat)
      private def patternImplies(x: Tree)(y: Tree): Boolean = (x, y) match {
        // since alternatives are flattened, must treat them as separate cases
        case (Alternative(pats), _) => pats.exists(p => patternImplies(p)(y))
        case (_, Alternative(pats)) => pats.exists(q => patternImplies(x)(q))
        // regular switch
        case (Literal(Constant(cx)), Literal(Constant(cy))) => cx == cy
        case (Ident(nme.WILDCARD), _)                       => true
        // type-switch for catch
        case (Bind(_, Typed(Ident(nme.WILDCARD), tpX)),
              Bind(_, Typed(Ident(nme.WILDCARD), tpY)))     => instanceOfTpImplies(tpY.tpe, tpX.tpe)
        case _ => false
      }

      private def noGuards(cs: List[CaseDef]): Boolean = !cs.exists(isGuardedCase)

      // must do this before removing guards from cases and collapsing (SI-6011, SI-6048)
      private def unreachableCase(cs: List[CaseDef]): Option[CaseDef] = {
        var cases = cs
        var unreachable: Option[CaseDef] = None

        while (cases.nonEmpty && unreachable.isEmpty) {
          val currCase = cases.head
          if (isDefault(currCase) && cases.tail.nonEmpty) // subsumed by the `else if` that follows, but faster
            unreachable = Some(cases.tail.head)
          else if (!isGuardedCase(currCase) || currCase.guard.tpe =:= ConstantType(Constant(true)))
            unreachable = cases.tail.find(caseImplies(currCase))
          else if (currCase.guard.tpe =:= ConstantType(Constant(false)))
            unreachable = Some(currCase)

          cases = cases.tail
        }

        unreachable
      }

      // empty list ==> failure
      def apply(cases: List[(Symbol, List[TreeMaker])], pt: Type): List[CaseDef] =
        // generate if-then-else for 1 case switch (avoids verify error... can't imagine a one-case switch being faster than if-then-else anyway)
        if (cases.isEmpty || cases.tail.isEmpty) Nil
        else {
          val caseDefs = cases map { case (scrutSym, makers) =>
            makers match {
              // default case
              case GuardAndBodyTreeMakers(guard, body) =>
                Some(defaultCase(scrutSym, guard, body))
              // constant (or typetest for typeSwitch)
              case SwitchableTreeMaker(pattern) :: GuardAndBodyTreeMakers(guard, body) =>
                Some(CaseDef(pattern, guard, body))
              // alternatives
              case AlternativesTreeMaker(_, altss, _) :: GuardAndBodyTreeMakers(guard, body) if alternativesSupported =>
                val switchableAlts = altss map {
                  case SwitchableTreeMaker(pattern) :: Nil =>
                    Some(pattern)
                  case _ =>
                    None
                }

                // succeed if they were all switchable
                sequence(switchableAlts) map { switchableAlts =>
                  CaseDef(Alternative(switchableAlts), guard, body)
                }
              case _ =>
                // patmatDebug("can't emit switch for "+ makers)
                None //failure (can't translate pattern to a switch)
            }
          }

          val caseDefsWithGuards = sequence(caseDefs) match {
            case None      => return Nil
            case Some(cds) => cds
          }

          val allReachable = unchecked || {
            // a switch with duplicate cases yields a verify error,
            // and a switch with duplicate cases and guards cannot soundly be rewritten to an unguarded switch
            // (even though the verify error would disappear, the behaviour would change)
            unreachableCase(caseDefsWithGuards) map (cd => reportUnreachable(cd.body.pos)) isEmpty
          }

          if (!allReachable) Nil
          else if (noGuards(caseDefsWithGuards)) {
            if (isDefault(caseDefsWithGuards.last)) caseDefsWithGuards
            else caseDefsWithGuards :+ defaultCase()
          } else {
            // collapse identical cases with different guards, push guards into body for all guarded cases
            // this translation is only sound if there are no unreachable (duplicate) cases
            // it should only be run if there are guarded cases, and on failure it returns Nil
            val (collapsed, needDefaultLabel) = collapseGuardedCases(caseDefsWithGuards)

            if (collapsed.isEmpty || (needDefaultLabel && !canJump)) Nil
            else {
              def wrapInDefaultLabelDef(cd: CaseDef): CaseDef =
                if (needDefaultLabel) deriveCaseDef(cd){ b =>
                  // TODO: can b.tpe ever be null? can't really use pt, see e.g. pos/t2683 or cps/match1.scala
                  defaultLabel setInfo MethodType(Nil, if (b.tpe != null) b.tpe else pt)
                  LabelDef(defaultLabel, Nil, b)
                } else cd

              val last = collapsed.last
              if (isDefault(last)) {
                if (!needDefaultLabel) collapsed
                else collapsed.init :+ wrapInDefaultLabelDef(last)
              } else collapsed :+ wrapInDefaultLabelDef(defaultCase())
            }
          }
        }
    }

    class RegularSwitchMaker(scrutSym: Symbol, matchFailGenOverride: Option[Tree => Tree], val unchecked: Boolean) extends SwitchMaker {
      val switchableTpe = Set(ByteClass.tpe, ShortClass.tpe, IntClass.tpe, CharClass.tpe)
      val alternativesSupported = true
      val canJump = true

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
      def defaultCase(scrutSym: Symbol = defaultSym, guard: Tree = EmptyTree, body: Tree = defaultBody): CaseDef = { import CODE._; atPos(body.pos) {
        (DEFAULT IF guard) ==> body
      }}
    }

    override def emitSwitch(scrut: Tree, scrutSym: Symbol, cases: List[List[TreeMaker]], pt: Type, matchFailGenOverride: Option[Tree => Tree], unchecked: Boolean): Option[Tree] = { import CODE._
      val regularSwitchMaker = new RegularSwitchMaker(scrutSym, matchFailGenOverride, unchecked)
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
      val unchecked = false
      def switchableTpe(tp: Type) = true
      val alternativesSupported = false // TODO: needs either back-end support of flattening of alternatives during typers
      val canJump = false

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
      def defaultCase(scrutSym: Symbol = defaultSym, guard: Tree = EmptyTree, body: Tree = defaultBody): CaseDef = { import CODE._; atPos(body.pos) {
        (CASE (Bind(scrutSym, Typed(Ident(nme.WILDCARD), TypeTree(ThrowableClass.tpe)))) IF guard) ==> body
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
        val matchEnd = newSynthCaseLabel("matchEnd")
        val matchRes = NoSymbol.newValueParameter(newTermName("x"), NoPosition, newFlags = SYNTHETIC) setInfo restpe.withoutAnnotations
        matchEnd setInfo MethodType(List(matchRes), restpe)

        def newCaseSym = newSynthCaseLabel("case") setInfo MethodType(Nil, restpe)
        var _currCase = newCaseSym

        val caseDefs = cases map { (mkCase: Casegen => Tree) =>
          val currCase = _currCase
          val nextCase = newCaseSym
          _currCase = nextCase

          LabelDef(currCase, Nil, mkCase(new OptimizedCasegen(matchEnd, nextCase)))
        }

        // must compute catchAll after caseLabels (side-effects nextCase)
        // catchAll.isEmpty iff no synthetic default case needed (the (last) user-defined case is a default)
        // if the last user-defined case is a default, it will never jump to the next case; it will go immediately to matchEnd
        val catchAllDef = matchFailGen map { matchFailGen =>
          val scrutRef = if(scrutSym ne NoSymbol) REF(scrutSym) else EmptyTree // for alternatives

          LabelDef(_currCase, Nil, matchEnd APPLY (matchFailGen(scrutRef)))
        } toList // at most 1 element

        // scrutSym == NoSymbol when generating an alternatives matcher
        val scrutDef = if(scrutSym ne NoSymbol) List(VAL(scrutSym)  === scrut) else Nil // for alternatives

        // the generated block is taken apart in TailCalls under the following assumptions
          // the assumption is once we encounter a case, the remainder of the block will consist of cases
          // the prologue may be empty, usually it is the valdef that stores the scrut
          // val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        Block(
          scrutDef ++ caseDefs ++ catchAllDef,
          LabelDef(matchEnd, List(matchRes), REF(matchRes))
        )
      }

      class OptimizedCasegen(matchEnd: Symbol, nextCase: Symbol) extends CommonCodegen with Casegen {
        def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree =
          optimizedCodegen.matcher(scrut, scrutSym, restpe)(cases, matchFailGen)

        // only used to wrap the RHS of a body
        // res: T
        // returns MatchMonad[T]
        def one(res: Tree): Tree = matchEnd APPLY (res) // a jump to a case label is special-cased in typedApply
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
          reportUnreachable(cases(caseIndex).last.pos)
        }
        val counterExamples = exhaustive(prevBinder, cases, pt)
        if (counterExamples.nonEmpty)
          reportMissingCases(prevBinder.pos, counterExamples)
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

object PatternMatchingStats {
  val patmatNanos         = Statistics.newTimer     ("time spent in patmat", "patmat")
  val patmatAnaDPLL       = Statistics.newSubTimer  ("  of which DPLL", patmatNanos)
  val patmatCNF           = Statistics.newSubTimer  ("  of which in CNF conversion", patmatNanos)
  val patmatCNFSizes      = Statistics.newQuantMap[Int, Statistics.Counter]("  CNF size counts", "patmat")(Statistics.newCounter(""))
  val patmatAnaVarEq      = Statistics.newSubTimer  ("  of which variable equality", patmatNanos)
  val patmatAnaExhaust    = Statistics.newSubTimer  ("  of which in exhaustivity", patmatNanos)
  val patmatAnaReach      = Statistics.newSubTimer  ("  of which in unreachability", patmatNanos)
}
