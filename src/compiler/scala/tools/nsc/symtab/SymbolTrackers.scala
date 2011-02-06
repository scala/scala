/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package symtab

/** Printing the symbol graph (for those symbols attached to an AST node)
 *  after each phase.
 */
trait SymbolTrackers {
  val global: Global
  import global._

  private implicit lazy val SymbolOrdering: Ordering[Symbol] =
    Ordering by (x => (x.kindString, x.name.toString))

  private implicit def toList[T: Ordering](xs: Set[T]): List[T] = xs.toList.sorted

  /** Reversing the direction of Symbol's owner arrow. */
  trait Hierarchy {
    def root: Symbol
    def children: List[Hierarchy]
    def flatten: Set[Symbol]
    def indentString(indent: String): String
    def symString(sym: Symbol): String

    override def toString() = indentString("")
  }
  case class Change(
    added: Set[Symbol],
    removed: Set[Symbol],
    changedOwner: Map[Symbol, Symbol]
  )

  object SymbolTracker {
    def containsSymbol(t: Tree) = t.symbol != null && t.symbol != NoSymbol

    def symbolsInUnit(unit: CompilationUnit): Set[Symbol] = {
      if (unit.body == null) Set.empty[Symbol]
      else unit.body filter containsSymbol map (_.symbol) toSet
    }
    def apply(unit: CompilationUnit) = new SymbolTracker(
      () => symbolsInUnit(unit) filterNot (_.ownerChain.exists(_ hasFlag Flags.SPECIALIZED))
    )
  }

  class SymbolTracker(sourceFn: () => Set[Symbol]) {
    private var history    = List[Change]()
    private var prev       = Set[Symbol]()
    private var prevOwners = Map[Symbol, Symbol]()
    private def changedOwner(sym: Symbol) = prevOwners get sym filter (_ != sym.owner)
    private def changedOwnerString(sym: Symbol) = changedOwner(sym) match {
      case Some(prev) => "[Owner changed: was " + ownersString(sym, 2) + "]"
      case _          => ""
    }
    private implicit def NodeOrdering: Ordering[Node] = Ordering by (_.root)
    private def ownersString(sym: Symbol, num: Int) = sym.ownerChain drop 1 take num mkString " -> "
    private def allOwnersString(sym: Symbol) = sym.ownerChain mkString " -> "
    private def isAdded(sym: Symbol) = history.nonEmpty && history.head.added(sym)
    private def isOwnerChange(sym: Symbol) = history.nonEmpty && (history.head.changedOwner contains sym)

    object Node {
      def nodes(syms: Set[Symbol]): List[Node] = {
        def descendents(s: Symbol) = (syms - s) filter (_ hasTransOwner s)
        def rooted(root: Symbol)   = new Node(root, nodes(descendents(root)))

        val roots    = syms filterNot (_.ownerChain drop 1 exists syms)
        val deep     = roots map rooted
        val deepSyms = deep flatMap (_.flatten)

        deep ++ (syms filterNot deepSyms map (x => Node(x)))
      }

      def apply(sym: Symbol): Node = new Node(sym, Nil)
      def apply(syms: Set[Symbol]): Node = nodes(syms) match {
        case List(x)  => x
        case xs       => new Node(NoSymbol, xs)
      }
    }
    class Node(val root: Symbol, val children: List[Hierarchy]) extends Hierarchy {
      def indicatorString = if (isAdded(root)) "* " else "  "

      def symString(sym: Symbol) = (
        sym + changedOwnerString(sym) + " " +
        sym.hasFlagsToString(Flags.PrintableFlags)
      )

      def flatten = children.foldLeft(Set(root))(_ ++ _.flatten)
      def indentString(indent: String): String = {
        if (root == NoSymbol)
          children map (c => c.indentString(indent)) mkString "\n"
        else {
          indicatorString + indent + symString(root) + (
            if (children.isEmpty) ""
            else children map (c => c.indentString(indent + "    ")) mkString ("\n", "\n", "")
          )
        }
      }
    }

    def snapshot(): Unit = {
      val syms    = sourceFn()
      val added   = syms filterNot prev
      val removed = prev filterNot syms
      val changed = ({
        for (sym <- prev intersect syms; old <- changedOwner(sym)) yield
          (sym, old)
      }).toMap

      val change = Change(added, removed, changed)
      prev       = syms
      prevOwners = syms map (s => (s, s.owner)) toMap;
      history    = change :: history
    }
    def show(): String = {
      val hierarchy = Node(sourceFn())
      val removed   = if (history.isEmpty) Set() else history.head.removed

      hierarchy.toString() + (
        if (removed.isEmpty) ""
        else removed map allOwnersString mkString (
          "\n\n!!! " + removed.size + " symbols vanished:\n", "\n", ""
        )
      )
    }
  }
}
