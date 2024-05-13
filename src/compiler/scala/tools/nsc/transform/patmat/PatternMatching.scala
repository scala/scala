/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.patmat

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Global
import scala.tools.nsc.ast
import scala.language.postfixOps
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.{Mode, Types}
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
trait PatternMatching extends Transform
                      with TypingTransformers
                      with Debugging
                      with Interface
                      with MatchTranslation
                      with MatchTreeMaking
                      with MatchCodeGen
                      with MatchCps
                      with ScalaLogic
                      with Solving
                      with MatchAnalysis
                      with MatchOptimization
                      with MatchWarnings
                      with PatternExpansion {
  import global._

  val phaseName: String = "patmat"

  def newTransformer(unit: CompilationUnit): Transformer = new MatchTransformer(unit)

  class MatchTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private var inAsync = false

    override def transform(tree: Tree): Tree = tree match {
      case dd: DefDef if async.hasAsyncAttachment(dd) =>
        val wasInAsync = inAsync
        try {
          inAsync = true
          super.transform(dd)
        } finally
          inAsync = wasInAsync

      case Match(sel, cases) =>
        val origTp = tree.tpe
        // setType origTp intended for CPS -- TODO: is it necessary?
        val translated = translator(sel.pos).translateMatch(treeCopy.Match(tree, transform(sel), transformTrees(cases).asInstanceOf[List[CaseDef]]))
        try {
          // Keep 2.12 behaviour of using wildcard expected type, recomputing the LUB, then throwing it away for the continuations plugins
          // but for the rest of us pass in top as the expected type to avoid waste.
          val pt = if (origTp <:< definitions.AnyTpe) definitions.AnyTpe else WildcardType
          localTyper.typed(translated, pt) match {
            case b @ Block(stats, m: Match) =>
              b.setType(origTp)
              m.setType(origTp)
              b
            case tree => tree setType origTp
          }
        } catch {
          case x: (Types#TypeError) =>
            // TODO: this should never happen; error should've been reported during type checking
            reporter.error(tree.pos, "error during expansion of this match (this is a scalac bug).\nThe underlying error was: "+ x.msg)
            translated
        }
      case Try(block, catches, finalizer) =>
        val selectorPos = catches.headOption.getOrElse(EmptyTree).orElse(finalizer).pos.focusEnd
        treeCopy.Try(tree, transform(block), translator(selectorPos).translateTry(transformTrees(catches).asInstanceOf[List[CaseDef]], tree.tpe, tree.pos), transform(finalizer))
      case _ => super.transform(tree)
    }

    // TODO: only instantiate new match translator when localTyper has changed
    // override def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A
    // as this is the only time TypingTransformer changes it
    def translator(selectorPos: Position): MatchTranslator with CodegenCore = {
      new OptimizingMatchTranslator(localTyper, selectorPos, inAsync)
    }
  }

  class PureMatchTranslator(val typer: analyzer.Typer, val matchStrategy: Tree, val selectorPos: Position) extends MatchTranslator with PureCodegen {
    def optimizeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, selectorPos: Position) = (cases, Nil)
    def analyzeCases(prevBinder: Symbol, cases: List[List[TreeMaker]], pt: Type, suppression: Suppression): Unit = {}
  }

  class OptimizingMatchTranslator(val typer: analyzer.Typer, val selectorPos: Position, val inAsync: Boolean)
    extends MatchTranslator
      with MatchOptimizer
      with MatchAnalyzer
      with Solver
}

trait Debugging {
  val global: Global

  // TODO: the inliner fails to inline the closures to debug.patmat unless the method is nested in an object
  object debug {
    val printPatmat = global.settings.Ypatmatdebug.value
    @inline final def patmat(s: => String) = if (printPatmat) Console.err.println(s)
    @inline final def patmatResult[T](s: => String)(result: T): T = {
      if (printPatmat) Console.err.println(s + ": " + result)
      result
    }
  }
}

trait Interface extends ast.TreeDSL {
  import global._
  import analyzer.Typer

  // 2.10/2.11 compatibility
  protected final def dealiasWiden(tp: Type)   = tp.dealiasWiden
  protected final def mkTRUE                   = CODE.TRUE
  protected final def mkFALSE                  = CODE.FALSE
  protected final def hasStableSymbol(p: Tree) = p.hasSymbolField && p.symbol.isStable

  object vpmName {
    val one       = newTermName("one")
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
   * if there's a <code>__match</code> in scope, we use this as the match strategy, assuming it conforms to MatchStrategy as defined below:

       {{{
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
       }}}

   * P and M are derived from one's signature (`def one[T](x: P[T]): M[T]`)


   * if no <code>__match</code> is found, we assume the following implementation (and generate optimized code accordingly)

       {{{
       object __match extends MatchStrategy[({type Id[x] = x})#Id, Option] {
         def zero = None
         def one[T](x: T) = Some(x)
         // NOTE: guard's return type must be of the shape M[T], where M is the monad in which the pattern match should be interpreted
         def guard[T](cond: Boolean, then: => T): Option[T] = if(cond) Some(then) else None
         def runOrElse[T, U](x: T)(f: T => Option[U]): U = f(x) getOrElse (throw new MatchError(x))
       }
       }}}

   */
  trait MatchMonadInterface {
    val typer: Typer
    val matchOwner = typer.context.owner
    def pureType(tp: Type): Type = tp

    def reportUnreachable(pos: Position) = typer.context.warning(pos, "unreachable code", WarningCategory.OtherMatchAnalysis)
    def reportMissingCases(pos: Position, counterExamples: List[String]) = {
      val ceString =
        if (counterExamples.tail.isEmpty) "input: " + counterExamples.head
        else "inputs: " + counterExamples.mkString(", ")

      typer.context.warning(pos, "match may not be exhaustive.\nIt would fail on the following "+ ceString, WarningCategory.OtherMatchAnalysis)
    }
  }


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// substitution
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait TypedSubstitution extends MatchMonadInterface {
    object Substitution {
      def apply(from: Symbol, to: Tree): Substitution = new Substitution(from :: Nil, to :: Nil)
      // requires sameLength(from, to)
      def apply(from: List[Symbol], to: List[Tree]): Substitution =
        if (from nonEmpty) new Substitution(from, to) else EmptySubstitution
    }

    class Substitution(val from: List[Symbol], val to: List[Tree]) {
      import global.{Transformer, Ident, NoType, TypeTree, SingleType}

      private def typedStable(t: Tree) = typer.typed(t.shallowDuplicate, Mode.MonoQualifierModes | Mode.TYPEPATmode)
      lazy val toTypes: List[Type] = to map (tree => typedStable(tree).tpe)

      // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
      // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
      def apply(tree: Tree): Tree = {
        // according to -Ystatistics 10% of translateMatch's time is spent in this method...
        // since about half of the typedSubst's end up being no-ops, the check below shaves off 5% of the time spent in typedSubst

        val checkType = new TypeCollector[Boolean](false) {
          def traverse(tp: Type) {
            if (!result) {
              tp match {
                case SingleType(_, sym) =>
                  if (from contains sym) {
                    global.devWarningIf(to.exists(!_.isInstanceOf[Ident])) {
                      s"Unexpected substitution of non-Ident into TypeTree, subst= $this"
                    }
                    result = true
                  }
                case _ =>
              }
              mapOver(tp)
            }
          }
        }
        val containsSym = tree.exists {
          case i@Ident(_) => from contains i.symbol
          case tt: TypeTree =>
            checkType.result = false
            checkType.collect(tt.tpe)
          case _          => false
        }

        object substIdentsForTrees extends Transformer {
          private def typedIfOrigTyped(to: Tree, origTp: Type): Tree =
            if (origTp == null || origTp == NoType) to
            // important: only type when actually substituting and when original tree was typed
            // (don't need to use origTp as the expected type, though, and can't always do this anyway due to unknown type params stemming from polymorphic extractors)
            else typer.typed(to)


          override def transform(tree: Tree): Tree = {
            def subst(from: List[Symbol], to: List[Tree]): Tree =
              if (from.isEmpty) tree
              else if (tree.symbol == from.head) typedIfOrigTyped(typedStable(to.head).setPos(tree.pos), tree.tpe)
              else subst(from.tail, to.tail)

            val tree1 = tree match {
              case Ident(_) => subst(from, to)
              case _        => super.transform(tree)
            }
            tree1 match {
              case _: DefTree =>
                tree1.symbol.modifyInfo(_.substituteTypes(from, toTypes))
              case _ =>
            }
            tree1.modifyType(_.substituteTypes(from, toTypes))
          }
        }
        if (containsSym) {
          if (to.forall(_.isInstanceOf[Ident]))
            tree.duplicate.substituteSymbols(from, to.map(_.symbol)) // scala/bug#7459 catches `case t => new t.Foo`
          else
            substIdentsForTrees.transform(tree)
        }
        else tree
      }


      // the substitution that chains `other` before `this` substitution
      // forall t: Tree. this(other(t)) == (this >> other)(t)
      def >>(other: Substitution): Substitution = if (other == EmptySubstitution) this else {
        // HOT
        val newFrom = new ListBuffer[Symbol]
        val newTo = new ListBuffer[Tree]
        foreach2(from, to) { (f, t) =>
          if (!other.from.contains(f)) {
            newFrom += f
            newTo += t
          }
        }
        new Substitution(newFrom.prependToList(other.from), newTo.prependToList(other.to.mapConserve(apply)))
      }
      override def toString = (from.map(_.name) zip to) mkString("Substitution(", ", ", ")")
    }

    object EmptySubstitution extends Substitution(Nil, Nil) {
      override def apply(tree: Tree): Tree = tree
      override def >>(other: Substitution): Substitution = other
    }
  }
}

trait PatternMatchingStats {
  self: Statistics =>
  val patmatNanos         = newTimer     ("time spent in patmat", "patmat")
  val patmatAnaDPLL       = newSubTimer  ("  of which DPLL", patmatNanos)
  val patmatCNF           = newSubTimer  ("  of which in CNF conversion", patmatNanos)
  val patmatCNFSizes      = newQuantMap[Int, Counter]("  CNF size counts", "patmat")(newCounter(""))
  val patmatAnaVarEq      = newSubTimer  ("  of which variable equality", patmatNanos)
  val patmatAnaExhaust    = newSubTimer  ("  of which in exhaustivity", patmatNanos)
  val patmatAnaReach      = newSubTimer  ("  of which in unreachability", patmatNanos)
}
