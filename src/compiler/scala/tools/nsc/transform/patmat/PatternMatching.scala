/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.Global
import scala.tools.nsc.ast
import scala.language.postfixOps
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.Types
import scala.reflect.internal.util.Position

/** Translate pattern matching.
  *
  * Either into optimized if/then/else's, or virtualized as method calls (these methods form a zero-plus monad),
  * similar in spirit to how for-comprehensions are compiled.
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
trait PatternMatching extends Transform with TypingTransformers
                      with Debugging
                      with Interface
                      with MatchTranslation
                      with MatchTreeMaking
                      with MatchCodeGen
                      with ScalaLogic
                      with Solving
                      with MatchAnalysis
                      with MatchOptimization {
  import global._

  val phaseName: String = "patmat"

  def newTransformer(unit: CompilationUnit): Transformer =
    if (opt.virtPatmat) new MatchTransformer(unit)
    else noopTransformer

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

    // TODO: only instantiate new match translator when localTyper has changed
    // override def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A
    // as this is the only time TypingTransformer changes it
    def translator: MatchTranslator with CodegenCore = {
      new OptimizingMatchTranslator(localTyper)
    }
  }

  class PureMatchTranslator(val typer: analyzer.Typer, val matchStrategy: Tree) extends MatchTranslator with PureCodegen {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type) = (cases, Nil)
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit = {}
  }

  class OptimizingMatchTranslator(val typer: analyzer.Typer) extends MatchTranslator
                                                             with MatchOptimizer
                                                             with MatchAnalyzer
                                                             with Solver
}

trait Debugging {
  val global: Global

  // TODO: the inliner fails to inline the closures to debug.patmat unless the method is nested in an object
  object debug {
    val printPatmat = global.settings.Ypatmatdebug.value
    @inline final def patmat(s: => String) = if (printPatmat) println(s)
  }
}

trait Interface extends ast.TreeDSL {
  import global.{newTermName, analyzer, Type, ErrorType, Symbol, Tree}
  import analyzer.Typer

  // 2.10/2.11 compatibility
  protected final def dealiasWiden(tp: Type)   = tp.dealias                       // 2.11: dealiasWiden
  protected final def mkTRUE                   = CODE.TRUE_typed                  // 2.11: CODE.TRUE
  protected final def mkFALSE                  = CODE.FALSE_typed                 // 2.11: CODE.FALSE
  protected final def hasStableSymbol(p: Tree) = p.hasSymbol && p.symbol.isStable // 2.11: p.hasSymbolField && p.symbol.isStable
  protected final def devWarning(str: String)  = global.debugwarn(str)            // 2.11: omit

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

    def counted(str: String, i: Int) = newTermName(str + i)
  }

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

    import global.definitions.repeatedToSeq;
    import global.elimAnonymousClass;
    def selectorType(selector: Tree): Type = pureType(repeatedToSeq(elimAnonymousClass(selector.tpe.widen.withoutAnnotations)))
    def inMatchMonad(tp: Type): Type
    def pureType(tp: Type): Type
    final def matchMonadResult(tp: Type): Type =
      tp.baseType(matchMonadSym).typeArgs match {
        case arg :: Nil => arg
        case _ => ErrorType
      }

    protected def matchMonadSym: Symbol
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
      import global.{Transformer, Ident, NoType}

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
            else typer.typed(to)

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
