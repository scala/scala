/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.{ast, symtab, typechecker, transform, Global}
import transform._
import typechecker._
import symtab._
import Flags.{MUTABLE, METHOD, LABEL, SYNTHETIC, ARTIFACT}
import scala.language.postfixOps
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.Types
import scala.reflect.internal.util.HashSet

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
trait PatternMatching extends Transform with TypingTransformers with ast.TreeDSL
                      with Debugging
                      with Interface
                      with Translation
                      with TreeMaking
                      with CodeGen
                      with Logic
                      with Analysis
                      with Optimization {

  import PatternMatchingStats._
  val global: Global
  import global._

  val phaseName: String = "patmat"

  def newTransformer(unit: CompilationUnit): Transformer = new MatchTransformer(unit)

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

  class PureMatchTranslator(val typer: analyzer.Typer, val matchStrategy: Tree) extends MatchTranslation with TreeMakers with PureCodegen
  class OptimizingMatchTranslator(val typer: analyzer.Typer)                    extends MatchTranslation with TreeMakers with MatchOptimizations
}

trait Debugging {
  val global: Global
  import global.settings

  // TODO: the inliner fails to inline the closures to patmatDebug
  object debugging {
    val printPatmat = settings.Ypatmatdebug.value
    @inline final def patmatDebug(s: => String) = if (printPatmat) println(s)
  }
}

trait Interface { self: ast.TreeDSL =>
  val global: Global
  import global.{newTermName, Position, Type, analyzer, ErrorType, Symbol, Tree}
  import analyzer.Typer

  // 2.10/2.11 compatibility
  protected final def dealiasWiden(tp: Type)   = tp.dealiasWiden
  protected final def mkTRUE                   = CODE.TRUE
  protected final def mkFALSE                  = CODE.FALSE
  protected final def hasStableSymbol(p: Tree) = p.hasSymbolField && p.symbol.isStable

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

    def inMatchMonad(tp: Type): Type
    def pureType(tp: Type): Type
    final def matchMonadResult(tp: Type): Type =
      tp.baseType(matchMonadSym).typeArgs match {
        case arg :: Nil => arg
        case _ => ErrorType
      }

    protected def matchMonadSym: Symbol
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
