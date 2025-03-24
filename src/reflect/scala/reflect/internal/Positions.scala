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

package scala
package reflect
package internal

import util._

/** Handling range positions
 *  atPos, the main method in this trait, will add positions to a tree,
 *  and will ensure the following properties:
 *
 *    1. All nodes between the root of the tree and nodes that already have positions
 *       will be assigned positions.
 *    2. No node which already has a position will be assigned a different range; however
 *       a RangePosition might become a TransparentPosition.
 *    3. The position of each assigned node includes the positions of each of its children.
 *    4. The positions of all solid descendants of children of an assigned node
 *       are mutually non-overlapping.
 *
 * Here, the solid descendant of a node are:
 *
 *   If the node has a TransparentPosition, the solid descendants of all its children
 *   Otherwise, the singleton consisting of the node itself.
 */
trait Positions extends api.Positions { self: SymbolTable =>
  type Position = scala.reflect.internal.util.Position
  val NoPosition = scala.reflect.internal.util.NoPosition
  implicit val PositionTag: ClassTag[Position] = ClassTag[Position](classOf[Position])

  def useOffsetPositions: Boolean = true

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position that is either focused or not.
   *  If the default point falls outside the calculated range, widen the result to include it.
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position = wrappingPos(default, trees, focus = true)

  private def wrappingPos(default: Position, trees: List[Tree], focus: Boolean): Position = if (useOffsetPositions) default else {
    // TODO: a tree's range position should cover the positions of all trees it "includes"
    // (inclusion mostly refers to subtrees, but also other attributes reached through the tree, such as its annotations/modifiers);
    // concretely, a MemberDef's position should cover its annotations (scala/bug#11060)
    // Workaround, which explicitly includes annotations of traversed trees, can be removed when TODO above is resolved:
    val accum = new WrappingPosAccumulator()
    def loop(trees: List[Tree]): Position = trees match {
      case head :: rest =>
        accum(head)
        head match {
          case md: MemberDef => loop(md.mods.annotations ::: rest)
          case _ => loop(rest)
        }
      case _ => accum.result(default, focus)
    }
    loop(trees)
  }
  private final class WrappingPosAccumulator extends (Tree => Boolean) {
    private[this] var min: Int = _
    private[this] var max: Int = _
    def reset(): Unit = {
      min = Int.MaxValue
      max = Int.MinValue
    }
    reset()
    def result(default: Position, focus: Boolean): Position =
      if (min > max) // there are no ranges
        if (focus) default.focus else default
      else {
        val point = default.pointOrElse(min)
        if (point < min || point > max) {
          val start = Math.min(min, point)
          val end   = Math.max(max, point)
          Position.range(default.source, start = start, point = point, end = end)
        }
        else
          Position.range(default.source, start = min, point = point, end = max)
      }
    override def apply(v1: Tree): Boolean = {
      val pos = v1.pos
      if (pos.isRange) {
        min = Math.min(min, pos.start)
        max = Math.max(max, pos.end)
      }
      true
    }
  }

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first tree's position.
   *  If some of the trees are non-synthetic, returns a range position enclosing the non-synthetic trees.
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position = {
    val headpos = trees.head.pos
    if (useOffsetPositions || !headpos.isDefined) headpos
    else wrappingPos(headpos, trees)
  }

  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range, assigning TransparentPositions
   *  to some of the nodes in `tree` or focusing on the position.
   */
  def ensureNonOverlapping(tree: Tree, others: List[Tree]): Unit = ensureNonOverlapping(tree, others, focus = true)
  def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean): Unit = if (!useOffsetPositions) {

    def isOverlapping(pos: Position) =
      pos.isRange && (others exists (pos overlaps _.pos))

    val treePos = tree.pos
    if (isOverlapping(treePos)) {
      val children = tree.children
      children foreach (ensureNonOverlapping(_, others, focus))
      if (treePos.isOpaqueRange) {
        val wpos = wrappingPos(treePos, children, focus)
        tree setPos (if (isOverlapping(wpos)) treePos.makeTransparent else wpos)
      }
    }
  }

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int): Position =
    if (useOffsetPositions) Position.offset(source, point)
    else Position.range(source, start, point, end)

  abstract class ChildSolidDescendantsCollector extends Traverser {
    // don't traverse annotations
    override def traverseModifiers(mods: Modifiers): Unit = ()

    override def traverse(tree: Tree): Unit =
      if (tree ne EmptyTree)
        if (tree.pos.isTransparent) super.traverse(tree)
        else traverseSolidChild(tree)

    def traverseSolidChild(t: Tree): Unit

    def apply(t: Tree): Unit = super.traverse(t)
  }

  private val posStartOrdering: Ordering[Tree] = new Ordering[Tree] {
    override def compare(x: Tree, y: Tree): Int = {
      def posOf(t: Tree): Int = {
        val pos = t.pos
        if (pos eq NoPosition) Int.MinValue else pos.start
      }
      Integer.compare(posOf(x), posOf(y))
    }
  }

  def validatePositions(tree: Tree): Unit = if (!useOffsetPositions) {
    def reportTree(prefix: String, tree: Tree): Unit = {
      val source = if (tree.pos.isDefined) tree.pos.source else ""
      inform("== " + prefix + " tree [" + tree.id + "] of type " + tree.productPrefix + " at " + tree.pos.show + source)
      inform("")
      inform(treeStatus(tree))
      inform("")
    }

    def positionError(topTree: Tree, msg: String)(body: => Unit): Unit = {
      inform("======= Position error\n" + msg)
      body
      inform("\nWhile validating #" + topTree.id)
      inform(treeStatus(topTree))
      inform("\nChildren:")
      topTree.children foreach (t => inform("  " + treeStatus(t, topTree)))
      inform("=======")
      throw new ValidateException(msg)
    }

    object worker {
      val trace = settings.Yposdebug.value && settings.verbose.value
      val topTree = tree

      object solidChildrenCollector extends ChildSolidDescendantsCollector {
        private[this] var size = 0
        private[this] var childSolidDescendants = new Array[Tree](32)
        private[this] val spares = new java.util.ArrayList[Array[Tree]]

        def borrowArray:  Array[Tree] = {
          val borrowed = childSolidDescendants
          childSolidDescendants = if (spares.isEmpty) new Array[Tree](32) else spares.remove(spares.size - 1)
          clear()
          borrowed
        }
        def spareArray(array: Array[Tree]): Unit = {
          spares.add(array)
        }

        def child(i:Int) = childSolidDescendants(i)
        def collectedSize = size
        def sortedArray: Array[Tree] = {
          if (size > 1)
            java.util.Arrays.sort(childSolidDescendants, 0, size, posStartOrdering)
          childSolidDescendants
        }

        //we dont care about zeroing the array
        def clear(): Unit = {size = 0}

        def traverseSolidChild(t: Tree): Unit = {
          if (size == childSolidDescendants.length) {
            spareArray(childSolidDescendants)
            childSolidDescendants = java.util.Arrays.copyOf(childSolidDescendants, size << 1)
          }
          childSolidDescendants(size) = t
          size += 1
        }
      }

      def loop(tree: Tree, encltree: Tree): Unit = if (!tree.isEmpty && tree.canHaveAttrs) {
        val treePos = tree.pos
        if (trace)
          inform(f"[${"validate"}%10s] ${treeStatus(tree, encltree)}")

        if (!treePos.isDefined)
          positionError(topTree, s"Unpositioned tree #${tree.id}") {
            inform("%15s %s".format("unpositioned", treeStatus(tree, encltree)))
            inform("%15s %s".format("enclosing", treeStatus(encltree)))
            encltree.children foreach (t => inform("%15s %s".format("sibling", treeStatus(t, encltree))))
          }

        solidChildrenCollector(tree)
        val numChildren = solidChildrenCollector.collectedSize

        if (treePos.isRange) {
          val enclPos = encltree.pos
          if (!enclPos.isRange)
            positionError(topTree, "Synthetic tree [" + encltree.id + "] contains nonsynthetic tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }
          if (!enclPos.includes(treePos))
            positionError(topTree, "Enclosing tree [" + encltree.id + "] does not include tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }

          if (numChildren > 1) {
            val childSolidDescendants = solidChildrenCollector.sortedArray
            var t1 = childSolidDescendants(0)
            var t1Pos = t1.pos
            var i = 1
            while (i < numChildren) {
              val t2 = childSolidDescendants(i)
              val t2Pos = t2.pos
              if (t1Pos.overlaps(t2Pos)) {
                positionError(topTree, "Overlapping trees") {
                  reportTree("Ancestor", tree)
                  reportTree("First overlapping", t1)
                  reportTree("Second overlapping", t2)
                }
              }
              if (t2Pos.isRange) { // only ranges overlap, so check ranges pairwise
                t1 = t2
                t1Pos = t2Pos
              }
              i += 1
            }
          }
        }
        if (numChildren > 0) {
          if (numChildren == 1) {
            val first = solidChildrenCollector.child(0)
            solidChildrenCollector.clear()
            loop(first, tree)
          } else {
            val snap = solidChildrenCollector.borrowArray
            var i = 0
            while (i < numChildren) {
              loop(snap(i), tree)
              i += 1
            }
            solidChildrenCollector.spareArray(snap)
          }
        }
      }
    }
    worker.loop(tree, tree)
  }

  /** Set position of all children of a node
   *  @param  pos   A target position.
   *                Uses the point of the position as the point of all positions it assigns.
   *                Uses the start of this position as an Offset position for unpositioned trees
   *                without children.
   *  @param  parent The parent of the child trees to position. All children must be positionable.
   */
  private def setChildrenPos(pos: Position, parent: Tree): Unit = try {
    setChildrenPosAccumulator.using(_.set(pos, parent))
  } catch {
    case ex: Exception =>
      inform("error while set children pos "+pos+" of "+parent.children)
      throw ex
  }
  private val setChildrenPosAccumulator = ReusableInstance[SetChildrenPosAccumulator](new SetChildrenPosAccumulator, enabled = isCompilerUniverse)
  private final class SetChildrenPosAccumulator extends (Tree => Boolean) {
    private[this] val wrappingPosAccumulator = new WrappingPosAccumulator
    private[this] var pos: Position = _
    def set(pos: Position, parent: Tree): Unit = {
      wrappingPosAccumulator.reset()
      this.pos = pos
      try parent.foreachChild(apply)
      finally {
        this.pos = null
      }
    }
    def apply(tree: Tree): Boolean = {
      wrappingPosAccumulator.reset()
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        tree.foreachChild(apply)
        tree.foreachChild(wrappingPosAccumulator)
        val wrappingPos = wrappingPosAccumulator.result(pos, focus = true)
        tree setPos wrappingPos
      }
      true
    }
  }

  class ValidateException(msg : String) extends Exception(msg)


  /** A locator for trees with given positions.
   *  Given a position `pos`, locator.apply returns
   *  the smallest tree that encloses `pos`.
   */
  class Locator(pos: Position) extends Traverser {
    var last: Tree = _
    def locateIn(root: Tree): Tree = {
      this.last = EmptyTree
      traverse(root)
      this.last
    }
    protected def isEligible(t: Tree) = !t.pos.isTransparent
    override def traverse(t: Tree): Unit = {
      t match {
        case tt : TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
          traverse(tt.original)
        case _ =>
          if (t.pos includes pos) {
            if (isEligible(t)) last = t
            super.traverse(t)
          }
          t match {
            case mdef: MemberDef =>
              val annTrees = mdef.mods.annotations match {
                case Nil if mdef.symbol != null =>
                  // After typechecking, annotations are moved from the modifiers
                  // to the annotation on the symbol of the annotatee.
                  mdef.symbol.annotations.map(_.original)
                case anns => anns
              }
              traverseTrees(annTrees)
            case _ =>
          }
      }
    }
  }

  class TypedLocator(pos: Position) extends Locator(pos) {
    override protected def isEligible(t: Tree) = super.isEligible(t) && t.tpe != null
  }

  trait PosAssigner extends InternalTraverser {
    var pos: Position
  }
  protected[this] lazy val posAssigner: PosAssigner = new DefaultPosAssigner

  protected class DefaultPosAssigner extends PosAssigner {
    var pos: Position = _
    override def traverse(t: Tree): Unit = {
      if (!t.canHaveAttrs) ()
      else if (t.pos == NoPosition) {
        t.setPos(pos)
        t.traverse(this)   // TODO: bug? shouldn't the traverse be outside of the if?
        // @PP: it's pruning whenever it encounters a node with a
        // position, which I interpret to mean that (in the author's
        // mind at least) either the children of a positioned node will
        // already be positioned, or the children of a positioned node
        // do not merit positioning.
        //
        // Whatever the author's rationale, it does seem like a bad idea
        // to press on through a positioned node to find unpositioned
        // children beneath it and then to assign whatever happens to
        // be in `pos` to such nodes. There are supposed to be some
        // position invariants which I can't imagine surviving that.
      }
    }
  }

  /** Position a tree.
   *  This means: Set position of a node and position all its unpositioned children.
   */
  def atPos[T <: Tree](pos: Position)(tree: T): tree.type = {
    if (useOffsetPositions || !pos.isOpaqueRange) {
      posAssigner.pos = pos
      posAssigner.traverse(tree)
    }
    else if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
      tree.setPos(pos)
      tree.onlyChild match {
        case EmptyTree =>
          setChildrenPos(pos, tree)
        case only =>
          atPos(pos)(only)
      }
    }
    tree
  }
}
