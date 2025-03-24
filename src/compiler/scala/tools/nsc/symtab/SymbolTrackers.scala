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

package scala.tools.nsc
package symtab

import scala.language.implicitConversions

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
    trees: Map[Symbol, Set[Tree]],  // symbol -> trees which proudly display it
    owners: Map[Symbol, Symbol],    // symbol -> previous owner
    flags: Map[Symbol, Long]        // symbol -> previous flags
  )

  object SymbolTracker {
    def containsSymbol(t: Tree) = t.symbol != null && t.symbol != NoSymbol

    // This is noise reduction only.
    def dropSymbol(sym: Symbol) = sym.ownerChain exists (_ hasFlag Flags.SPECIALIZED)

    def symbolSnapshot(unit: CompilationUnit): Map[Symbol, Set[Tree]] = {
      if (unit.body == null) Map.empty
      else unit.body.filter(containsSymbol).groupBy(_.symbol).view.mapValues(_.toSet).toMap
    }
    def apply(unit: CompilationUnit) = new SymbolTracker(
      () => symbolSnapshot(unit) filterNot { case (k, _) => dropSymbol(k) }
    )
  }

  class SymbolTracker(snapshotFn: () => Map[Symbol, Set[Tree]]) {
    def flagsMask: Long = Flags.PrintableFlags

    private var currentMap = Map[Symbol, Set[Tree]]()
    private var prevMap    = Map[Symbol, Set[Tree]]()
    private def current    = currentMap.keySet
    private def prev       = prevMap.keySet

    private var history    = List[Change](Change(Set(), Set(), Map(), Map(), Map()))
    private var prevFlags  = Map[Symbol, Long]()
    private var prevOwners = Map[Symbol, Symbol]()

    private def changed                    = history.head
    private def isAdded(sym: Symbol)       = changed added sym
    private def isOwnerChange(sym: Symbol) = changed.owners contains sym
    private def isFlagsChange(sym: Symbol) = changed.flags contains sym

    private implicit def NodeOrdering: Ordering[Node] = Ordering by (_.root)

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
      def masked = root.flags & flagsMask
      def indicatorString =
        if (isAdded(root)) "* "
        else List(
          if (isFlagsChange(root)) "F" else "",
          if (isOwnerChange(root)) "O" else "",
          "  "
        ).mkString take 2

      def changedOwnerString = changed.owners get root match {
        case Some(prev) => " [Owner was " + prev + ", now " + root.owner + "]"
        case _          => ""
      }
      def flagSummaryString = changed.flags get root match {
        case Some(oldFlags) =>
          val added   = masked & ~oldFlags
          val removed = oldFlags & ~masked
          val all     = masked | oldFlags
          val strs    = 0 to 63 map { bit =>
            val flag = 1L << bit
            val prefix = (
              if ((added & flag) != 0L) "+"
              else if ((removed & flag) != 0L) "-"
              else ""
            )
            if ((all & flag) == 0L) ""
            else prefix + Flags.flagToString(flag)
          }

          " " + strs.filterNot(_ == "").mkString("[", " ", "]")
        case _ =>
          if (masked == 0L) ""
          else " (" + Flags.flagsToString(masked) + ")"
      }
      def symString(sym: Symbol) = (
        if (settings.isDebug && sym.hasCompleteInfo) {
          val s = sym.defString take 240
          if (s.length == 240) s + "..." else s
        }
        else "" + sym + changedOwnerString + flagSummaryString
      )

      def flatten = children.foldLeft(Set(root))(_ ++ _.flatten)
      def indentString(indent: String): String = {
        if (root == NoSymbol)
          children map (c => c.indentString(indent)) mkString "\n"
        else {
          indicatorString + indent + symString(root) + (
            if (children.isEmpty) ""
            else children.map(_.indentString(indent + "    ")).mkString("\n", "\n", "")
          )
        }
      }
    }

    def snapshot(): Unit = {
      currentMap = snapshotFn()

      val added   = current filterNot prev
      val removed = prev filterNot current
      val steady  = prev intersect current

      def changedOwner(sym: Symbol) = prevOwners get sym filter (_ != sym.owner)
      def changedFlags(sym: Symbol) = prevFlags get sym filter (_ != (sym.flags & flagsMask))

      val owners = ({
        for (sym <- steady; old <- changedOwner(sym)) yield
          (sym, old)
      }).toMap
      val flags = ({
        for (sym <- steady; old <- changedFlags(sym)) yield
          (sym, old)
      }).toMap

      val change = Change(added, removed, prevMap, owners, flags)

      prevMap    = currentMap
      prevOwners = current.map(s => (s, s.owner)).toMap
      prevFlags  = current.map(s => (s, (s.flags & flagsMask))).toMap
      history    = change :: history
    }
    def show(label: String): String = {
      val hierarchy = Node(current)
      val Change(_, removed, symMap, _, _) = history.head
      def detailString(sym: Symbol) = {
        val ownerString = {
          val (few, rest) = sym.ownersIterator.splitAt(3)
          val ellipsis = Iterator("...").filter(_ => rest.hasNext)
          few.map(_.toString).concat(ellipsis).mkString(" -> ")
        }
        val treeStrings = symMap(sym).map(t => f"${t.shortClass}%10s: $t")

        (ownerString :: treeStrings).mkString("\n")
      }
      def removedString = (removed: List[Symbol]).zipWithIndex.map {
        case (t, i) => "(%2s) ".format(i + 1) + detailString(t)
      }.mkString("\n")

      "" + hierarchy + (
        if (removed.isEmpty) ""
        else "\n\n!!! " + label + ", " + removed.size + " symbols vanished:\n" + removedString
      )
    }
  }
}
