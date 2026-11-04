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

package scala.tools.nsc.transform.patmat

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
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
    private var inForceDesugar = false

    override def transform(tree: Tree): Tree = tree match {
      case dd: DefDef if dd.hasAttachment[ForceMatchDesugar.type] || dd.symbol.hasAttachment[ForceMatchDesugar.type] =>
        val wasInForceDesugar = inForceDesugar
        try {
          inForceDesugar = true
          super.transform(dd)
        } finally
          inForceDesugar = wasInForceDesugar

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
      new OptimizingMatchTranslator(localTyper, selectorPos, inForceDesugar)
    }

  }


  class OptimizingMatchTranslator(val typer: analyzer.Typer, val selectorPos: Position, val inForceDesugar: Boolean)
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
      def apply(from: Symbol, to: Tree): Substitution = new ListSubstitution(from :: Nil, to :: Nil)
      // requires sameLength(from, to)
      def apply(from: List[Symbol], to: List[Tree]): Substitution =
        if (from.isEmpty) EmptySubstitution
        else if (from.lengthCompare(IndexAbove) <= 0) new ListSubstitution(from, to)
        else IndexedSubstitution(from, to)

      // Substitutions with more bindings than this are an `IndexedSubstitution`. Building and updating its maps
      // costs more than the linear steps of a `ListSubstitution` save until it's this large.
      final val IndexAbove = 64
    }

    // Substitutions are composed with `>>` for each tree maker, and the composition mostly shares the `to` trees
    // of its parts. Remember their types (for the match being translated), rather than typing them again for each
    // composition, which made the analysis of a match quadratic in its number of cases.
    // Only used for lookups, keyed by identity; allocated on first use.
    private[this] var stableTypes: java.util.IdentityHashMap[Tree, Type] = null
    private def typedStable(t: Tree) = typer.typed(t.shallowDuplicate, Mode.MonoQualifierModes | Mode.TYPEPATmode)
    private def typedStableType(t: Tree): Type = {
      if (stableTypes eq null) stableTypes = new java.util.IdentityHashMap[Tree, Type]()
      val cached = stableTypes.get(t)
      if (cached ne null) cached
      else {
        val tp = typedStable(t).tpe
        stableTypes.put(t, tp)
        tp
      }
    }

    /** Substitutes the trees `to` for the symbols `from` (the first binding of a symbol wins). */
    sealed abstract class Substitution {
      def from: List[Symbol]
      def to: List[Tree]
      def size: Int

      def apply(tree: Tree): Tree

      // the substitution that chains `other` before `this` substitution
      // forall t: Tree. this(other(t)) == (this >> other)(t)
      def >>(other: Substitution): Substitution

      override def toString = from.map(_.name).zip(to).mkString("Substitution(", ", ", ")")
    }

    /** The bindings that `substitute` substitutes with, and the types of the `to` trees, computed on demand */
    private[TypedSubstitution] sealed trait SubstBindings {
      def from: List[Symbol]
      def to: List[Tree]
      def toTypes: List[Type]
    }

    class ListSubstitution(val from: List[Symbol], val to: List[Tree]) extends Substitution with SubstBindings {
      lazy val toTypes: List[Type] = to map typedStableType
      private[this] lazy val toIdents: Boolean = to.forall(_.isInstanceOf[global.Ident])

      def size = from.length // at most `IndexAbove`, unless this is the `EmptySubstitution`

      def apply(tree: Tree): Tree = substitute(tree, this, toIdents, this)

      def >>(other: Substitution): Substitution =
        if (other == EmptySubstitution) this
        else if (size + other.size > Substitution.IndexAbove) IndexedSubstitution(from, to) >> other
        else {
          // HOT
          val newFrom = new ListBuffer[Symbol]
          val newTo = new ListBuffer[Tree]
          foreach2(from, to) { (f, t) =>
            if (!other.from.contains(f)) {
              newFrom += f
              newTo += t
            }
          }
          new ListSubstitution(newFrom.prependToList(other.from), newTo.prependToList(other.to.mapConserve(apply)))
        }
    }

    object EmptySubstitution extends ListSubstitution(Nil, Nil) {
      override def apply(tree: Tree): Tree = tree
      override def >>(other: Substitution): Substitution = other
    }

    private final class Binding(val from: Symbol, val to: Tree)

    private final class RestrictedBindings(val from: List[Symbol], val to: List[Tree]) extends SubstBindings {
      lazy val toTypes: List[Type] = to map typedStableType
    }

    private object IndexedSubstitution {
      def apply(from: List[Symbol], to: List[Tree]): IndexedSubstitution =
        new IndexedSubstitution(immutable.TreeMap.empty, immutable.HashMap.empty, nonIdents = 0, nextKey = -1L).prepend(from, to)
    }

    /* The substitutions accumulated by the analysis, and in the translation of a case, grow with the number of
     * (sub)patterns: in the analysis, with the number of cases of the match. A tree typically mentions only one or
     * two of their symbols, but substituting with all of them made `apply` linear in the size of the substitution,
     * in `containsSym` and in the `SubstMap`s, which compare each type they visit to all of `from`, as did `>>`,
     * which copied the substitution.
     *
     * An `IndexedSubstitution` is persistent: `>>` updates (copies of) its maps in time logarithmic in its size.
     * `apply` first finds the bindings of the symbols that substituting all of `from` might compare to a symbol of
     * `from` (see `OccurringFromSymbols`), and substitutes with those only, in the same order.
     *
     * @param bindings  the bindings in the order of `from`, keyed by a number increasing along `from`
     * @param keys      the (ascending) keys of the bindings of a symbol, for lookups only
     * @param nonIdents the number of `to` trees that aren't an `Ident`
     * @param nextKey   a number smaller than any key
     */
    private final class IndexedSubstitution(bindings: immutable.TreeMap[Long, Binding], keys: immutable.HashMap[Symbol, List[Long]],
                                            nonIdents: Int, nextKey: Long) extends Substitution {
      lazy val from: List[Symbol] = bindings.valuesIterator.map(_.from).toList
      lazy val to: List[Tree] = bindings.valuesIterator.map(_.to).toList
      def size = bindings.size

      // `substitute` decides how to substitute based on all of `to`, even when it only gets the bindings relevant
      // to the tree at hand
      private def toIdents = nonIdents == 0

      private def keyOf(sym: Symbol): Long = keys.getOrElse(sym, Nil) match {
        case k :: _ => k
        case _      => Long.MaxValue
      }

      // `substitute`'s `containsSym` for all of `from`, with hashed lookups. If it's false, `substitute` would return
      // `tree`; otherwise, it would substitute, visiting (at least) what `OccurringFromSymbols` visits.
      private def containsSym(tree: Tree): Boolean = {
        import global.{Ident, TypeTree, SingleType}
        val checkType = new TypeCollector[Boolean](initial = false) {
          override def apply(tp: Type): Unit =
            if (!result) {
              tp match {
                case SingleType(_, sym) if keys.contains(sym) => result = true
                case _                                        => tp.foldOver(this)
              }
            }
        }
        tree.exists {
          case i: Ident     => keys.contains(i.symbol)
          case tt: TypeTree =>
            checkType.result = false
            checkType.collect(tt.tpe)
          case _            => false
        }
      }

      def apply(tree: Tree): Tree =
        if (!containsSym(tree)) tree
        else {
          val occurring = new OccurringFromSymbols
          occurring.collect(tree)
          val restrictedFrom = new ListBuffer[Symbol]
          val restrictedTo = new ListBuffer[Tree]
          occurring.keys.sortInPlace().foreach { k =>
            val b = bindings(k)
            restrictedFrom += b.from
            restrictedTo += b.to
          }
          substitute(tree, new RestrictedBindings(restrictedFrom.toList, restrictedTo.toList), toIdents, this)
        }

      def >>(other: Substitution): Substitution =
        if (other == EmptySubstitution) this
        else {
          // drop the bindings `other` overrides
          var bindings1 = bindings
          var keys1 = keys
          var nonIdents1 = nonIdents
          other.from.foreach { f =>
            keys1.getOrElse(f, Nil).foreach { k =>
              if (!bindings1(k).to.isInstanceOf[global.Ident]) nonIdents1 -= 1
              bindings1 -= k
            }
            keys1 -= f
          }
          new IndexedSubstitution(bindings1, keys1, nonIdents1, nextKey).prepend(other.from, other.to.mapConserve(apply))
        }

      // requires: `from` doesn't bind any symbol of this substitution
      private def prepend(from: List[Symbol], to: List[Tree]): IndexedSubstitution = {
        var bindings1 = bindings
        var keys1 = keys
        var nonIdents1 = nonIdents
        val firstKey = nextKey - from.length + 1
        var key = firstKey
        foreach2(from, to) { (f, t) =>
          bindings1 = bindings1.updated(key, new Binding(f, t))
          keys1 = keys1.updated(f, keys1.getOrElse(f, Nil) :+ key)
          if (!t.isInstanceOf[global.Ident]) nonIdents1 += 1
          key += 1
        }
        new IndexedSubstitution(bindings1, keys1, nonIdents1, firstKey - 1)
      }

      /* Finds the keys of the bindings of the symbols that substituting all of `from` into `tree` might compare to
       * a symbol of `from`. Substituting with these bindings only has the same result. Over-approximates what
       * `substitute` visits:
       *  - the symbols of the trees, and their types;
       *  - the infos of the symbols of definitions, and of their module classes, which `substituteSymbols` and
       *    `substIdentsForTrees` substitute into;
       *  - within types, everything `TypeMap.mapOver` visits (the `SubstMap`s use it, hence this is a `TypeMap`),
       *    including the trees in annotations. It records the symbols that `SubstMap` and `SubstSymMap` compare to
       *    `from`: those of type references and singleton types (with or without a prefix), the owner of the class
       *    of a refinement, and the symbols bound by method, poly and existential types (`renameBoundSyms`). Like
       *    `SubstMap`, it visits the parents of class infos once more;
       *  - the trees and types the substitution introduces, as it may visit them again (for example, the info of a
       *    definition whose symbol is substituted, or the declarations of a refinement after their definitions
       *    were substituted).
       */
      private final class OccurringFromSymbols extends TypeTraverser {
        val keys = mutable.ArrayBuffer.empty[Long]
        private[this] val seen = mutable.HashSet.empty[Long] // only for lookups
        private[this] var pending: List[Binding] = Nil

        private def record(sym: Symbol): Unit = if (sym ne null) {
          val k = keyOf(sym)
          if (k != Long.MaxValue && seen.add(k)) {
            keys += k
            pending ::= bindings(k)
          }
        }

        def traverse(tp: Type): Unit = if (tp ne null) {
          tp match {
            case TypeRef(_, sym, _)           => record(sym)
            case SingleType(_, sym)           => record(sym)
            case MethodType(params, _)        => params.foreach(record)
            case PolyType(tparams, _)         => tparams.foreach(record)
            case ExistentialType(qs, _)       => qs.foreach(record)
            case ClassInfoType(parents, _, _) => parents.foreach(traverse)
            case rt: RefinedType              => record(rt.typeSymbol.owner)
            case _                            =>
          }
          tp.mapOver(this)
        }

        private def traverseInfo(sym: Symbol): Unit = {
          traverse(sym.info)
          if (sym.isModule) traverse(sym.moduleClass.info)
        }

        private object trees extends global.InternalTraverser {
          override def traverse(t: Tree): Unit = {
            record(t.symbol)
            OccurringFromSymbols.this.traverse(t.tpe)
            t match {
              case _: DefTree if t.symbol ne null =>
                traverseInfo(t.symbol)
                if (toIdents) {
                  // `substituteSymbols` substitutes into the info of the new symbol of the definition
                  val k = keyOf(t.symbol)
                  if (k != Long.MaxValue) traverseInfo(bindings(k).to.symbol)
                }
              case _ =>
            }
            t.traverse(this)
          }
        }

        override def mapOver(tree: Tree, giveup: () => Nothing): Tree = { trees.traverse(tree); tree }

        def collect(tree: Tree): Unit = {
          trees.traverse(tree)
          while (pending.nonEmpty) {
            val b = pending.head
            pending = pending.tail
            trees.traverse(b.to)
            if (!toIdents) traverse(typedStableType(b.to))
          }
        }
      }
    }

    // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
    // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
    // `toIdents` is whether all `to` trees of `subst` (which may have more than `to`) are `Ident`s.
    private def substitute(tree: Tree, bindings: SubstBindings, toIdents: Boolean, subst: Substitution): Tree = {
      import global.{AstTransformer, Ident, NoType, TypeTree, SingleType}
      // according to -Ystatistics 10% of translateMatch's time is spent in this method...
      // since about half of the typedSubst's end up being no-ops, the check below shaves off 5% of the time spent in typedSubst
      val from = bindings.from
      val to = bindings.to

      val checkType = new TypeCollector[Boolean](initial = false) {
        override def apply(tp: Type): Unit =
          if (!result) {
            tp match {
              case SingleType(_, sym) if from contains sym =>
                global.devWarningIf(!toIdents) {
                  s"Unexpected substitution of non-Ident into TypeTree, subst= $subst"
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
              tree1.symbol.modifyInfo(_.substituteTypes(from, bindings.toTypes))
            case _ =>
          }
          tree1.modifyType(_.substituteTypes(from, bindings.toTypes))
        }
      }
      if (containsSym) {
        if (toIdents)
          tree.duplicate.substituteSymbols(from, to.map(_.symbol)) // scala/bug#7459 catches `case t => new t.Foo`
        else
          substIdentsForTrees.transform(tree)
      }
      else tree
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
