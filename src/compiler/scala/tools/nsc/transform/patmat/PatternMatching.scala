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

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.{Mode, Types}
import scala.reflect.internal.util.{SourceFile, Statistics}
import scala.tools.nsc.Global
import scala.tools.nsc.ast
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.Reporting.WarningCategory

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

  /** Symbols to force for determining children of sealed Java classes. */
  val javaClassesByUnit = perRunCaches.newMap[SourceFile, mutable.Set[Symbol]]()

  def newTransformer(unit: CompilationUnit): AstTransformer = new MatchTransformer(unit)

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

      case CaseDef(UnApply(Apply(Select(qual, nme.unapply), Ident(nme.SELECTOR_DUMMY) :: Nil), (bind@Bind(name, Ident(nme.WILDCARD))) :: Nil), guard, body)
        if guard.isEmpty && qual.symbol == definitions.NonFatalModule =>
        transform(treeCopy.CaseDef(
          tree,
          treeCopy.Bind(bind, name, Typed(Ident(nme.WILDCARD), TypeTree(definitions.ThrowableTpe)).setType(definitions.ThrowableTpe)),
          localTyper.typed(atPos(tree.pos)(Apply(gen.mkAttributedRef(definitions.NonFatal_apply), List(Ident(bind.symbol))))),
          body))

      case Match(sel, cases) =>
        val origTp = tree.tpe

        // setType origTp intended for CPS -- TODO: is it necessary?
        val translated = translator(sel.pos).translateMatch(treeCopy.Match(tree, transform(sel), transformCaseDefs(cases)))
        try {
          // Keep 2.12 behaviour of using wildcard expected type, recomputing the LUB, then throwing it away for the continuations plugins
          // but for the rest of us pass in top as the expected type to avoid waste.
          val pt = if (origTp <:< definitions.AnyTpe) definitions.AnyTpe else WildcardType
          localTyper.typed(translated, pt) match {
            case b @ Block(_, m: Match) =>
              b.setType(origTp)
              m.setType(origTp)
              b
            case t => t.setType(origTp)
          }
        } catch {
          case x: Types#TypeError =>
            // TODO: this should never happen; error should've been reported during type checking
            reporter.error(tree.pos, s"error during expansion of this match (this is a scalac bug).\nThe underlying error was: ${x.msg}")
            translated
        }
      case Try(block, catches, finalizer) =>
        val selectorPos = catches.headOption.getOrElse(EmptyTree).orElse(finalizer).pos.focusEnd
        treeCopy.Try(tree, transform(block), translator(selectorPos).translateTry(transformCaseDefs(catches), tree.tpe, tree.pos), transform(finalizer))
      case _ => super.transform(tree)
    }

    def translator(selectorPos: Position): MatchTranslator with CodegenCore = {
      new OptimizingMatchTranslator(localTyper, selectorPos, inAsync)
    }

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

  trait MatchMonadInterface {
    val typer: Typer
    val matchOwner = typer.context.owner

    def reportUnreachable(pos: Position) = typer.context.warning(pos, "unreachable code", WarningCategory.OtherMatchAnalysis)
    def reportMissingCases(pos: Position, counterExamples: List[String]) = {
      val ceString = counterExamples match {
        case Nil        => "" // never occurs, but not carried in the type
        case "_" :: Nil => ""
        case ex :: Nil  => s"\nIt would fail on the following input: $ex"
        case exs        => s"\nIt would fail on the following inputs: ${exs.mkString(", ")}"
      }

      typer.context.warning(pos, s"match may not be exhaustive.$ceString", WarningCategory.OtherMatchAnalysis)
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
        if (from.isEmpty) EmptySubstitution else new Substitution(from, to)
    }

    class Substitution(val from: List[Symbol], val to: List[Tree]) {
      import global.{AstTransformer, Ident, NoType, TypeTree, SingleType}

      private def typedStable(t: Tree) = typer.typed(t.shallowDuplicate, Mode.MonoQualifierModes | Mode.TYPEPATmode)
      lazy val toTypes: List[Type] = to map (tree => typedStable(tree).tpe)

      // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
      // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
      def apply(tree: Tree): Tree = {
        // according to -Ystatistics 10% of translateMatch's time is spent in this method...
        // since about half of the typedSubst's end up being no-ops, the check below shaves off 5% of the time spent in typedSubst

        val checkType = new TypeCollector[Boolean](initial = false) {
          override def apply(tp: Type): Unit =
            if (!result) {
              tp match {
                case SingleType(_, sym) if from contains sym =>
                  global.devWarningIf(to.exists(!_.isInstanceOf[Ident])) {
                    s"Unexpected substitution of non-Ident into TypeTree, subst= $this"
                  }
                  result = true
                case _ =>
                    tp.foldOver(this)
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

        object substIdentsForTrees extends AstTransformer {
          private def typedIfOrigTyped(to: Tree, origTp: Type): Tree =
            if (origTp == null || origTp == NoType) to
            // important: only type when actually substituting and when original tree was typed
            // (don't need to use origTp as the expected type, though, and can't always do this anyway due to unknown type params stemming from polymorphic extractors)
            else typer.typed(to)


          override def transform(tree: Tree): Tree = {
            @tailrec
            def subst(from: List[Symbol], to: List[Tree]): Tree =
              if (from.isEmpty) tree
              else if (tree.symbol == from.head) typedIfOrigTyped(typedStable(to.head).setPos(tree.pos), tree.tpe)
              else subst(from.tail, to.tail)

            val tree1 = tree match {
              case Ident(_) => subst(from, to)
              case _        => tree.transform(this)
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
      override def toString = from.map(_.name).zip(to).mkString("Substitution(", ", ", ")")
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
