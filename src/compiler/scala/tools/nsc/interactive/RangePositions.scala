/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import ast.Trees
import ast.Positions
import scala.reflect.internal.util.{SourceFile, Position, RangePosition, NoPosition}
import scala.tools.nsc.util.WorkScheduler
import scala.collection.mutable.ListBuffer

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
trait RangePositions extends Trees with Positions {
self: scala.tools.nsc.Global =>

  case class Range(pos: Position, tree: Tree) {
    def isFree = tree == EmptyTree
  }

  override def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new RangePosition(source, start, point, end)

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position that is either focused or not.
   */
  override def wrappingPos(default: Position, trees: List[Tree], focus: Boolean): Position = {
    val ranged = trees filter (_.pos.isRange)
    if (ranged.isEmpty) if (focus) default.focus else default
    else new RangePosition(default.source, (ranged map (_.pos.start)).min, default.point, (ranged map (_.pos.end)).max)
  }

  /** A position that wraps a non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns first tree's position.
   */
  override def wrappingPos(trees: List[Tree]): Position = {
    val headpos = trees.head.pos
    if (headpos.isDefined) wrappingPos(headpos, trees) else headpos
  }

  // -------------- ensuring no overlaps -------------------------------
  
  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range, assigning TransparentPositions
   *  to some of the nodes in `tree` or focusing on the position.
   */
  override def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {
    def isOverlapping(pos: Position) =
      pos.isRange && (others exists (pos overlaps _.pos))
    if (isOverlapping(tree.pos)) {
      val children = tree.children
      children foreach (ensureNonOverlapping(_, others, focus))
      if (tree.pos.isOpaqueRange) {
        val wpos = wrappingPos(tree.pos, children, focus)
        tree setPos (if (isOverlapping(wpos)) tree.pos.makeTransparent else wpos)
      }
    }
  }

  def solidDescendants(tree: Tree): List[Tree] =
    if (tree.pos.isTransparent) tree.children flatMap solidDescendants
    else List(tree)

  /** A free range from `lo` to `hi` */
  private def free(lo: Int, hi: Int): Range =
    Range(new RangePosition(null, lo, lo, hi), EmptyTree)

  /** The maximal free range */
  private lazy val maxFree: Range = free(0, Int.MaxValue)

  /** A singleton list of a non-empty range from `lo` to `hi`, or else the empty List */
  private def maybeFree(lo: Int, hi: Int) =
    if (lo < hi) List(free(lo, hi))
    else List()

  /** Insert `pos` into ranges `rs` if possible;
   *  otherwise add conflicting trees to `conflicting`.
   */
  private def insert(rs: List[Range], t: Tree, conflicting: ListBuffer[Tree]): List[Range] = rs match {
    case List() =>
      assert(conflicting.nonEmpty)
      rs
    case r :: rs1 =>
      assert(!t.pos.isTransparent)
      if (r.isFree && (r.pos includes t.pos)) {
//      println("subdividing "+r+"/"+t.pos)
        maybeFree(t.pos.end, r.pos.end) ::: List(Range(t.pos, t)) ::: maybeFree(r.pos.start, t.pos.start) ::: rs1
      } else {
        if (!r.isFree && (r.pos overlaps t.pos)) conflicting += r.tree
        r :: insert(rs1, t, conflicting)
      }
  }

  /** Replace elem `t` of `ts` by `replacement` list. */
  private def replace(ts: List[Tree], t: Tree, replacement: List[Tree]): List[Tree] =
    if (ts.head == t) replacement ::: ts.tail
    else ts.head :: replace(ts.tail, t, replacement)

  /** Does given list of trees have mutually non-overlapping positions?
   *  pre: None of the trees is transparent
   */
  def findOverlapping(cts: List[Tree]): List[(Tree, Tree)] = {
    var ranges = List(maxFree)
    for (ct <- cts) {
      if (ct.pos.isOpaqueRange) {
        val conflicting = new ListBuffer[Tree]
        ranges = insert(ranges, ct, conflicting)
        if (conflicting.nonEmpty) return conflicting.toList map (t => (t, ct))
      }
    }
    List()
  }

  // -------------- setting positions -------------------------------

  /** Set position of all children of a node
   *  @param  pos   A target position.
   *                Uses the point of the position as the point of all positions it assigns.
   *                Uses the start of this position as an Offset position for unpositioed trees
   *                without children.
   *  @param  trees  The children to position. All children must be positionable.
   */
  private def setChildrenPos(pos: Position, trees: List[Tree]): Unit = try {
    for (tree <- trees) {
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        val children = tree.children
        if (children.isEmpty) {
          tree setPos pos.focus
        } else {
          setChildrenPos(pos, children)
          tree setPos wrappingPos(pos, children)
        }
      }
    }
  } catch {
    case ex: Exception =>
      println("error while set children pos "+pos+" of "+trees)
      throw ex
  }

  /** Position a tree.
   *  This means: Set position of a node and position all its unpositioned children.
   */
  override def atPos[T <: Tree](pos: Position)(tree: T): T = {
    if (pos.isOpaqueRange) {
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        tree.setPos(pos)
        val children = tree.children
        if (children.nonEmpty) {
          if (children.tail.isEmpty) atPos(pos)(children.head)
          else setChildrenPos(pos, children)
        }
      }
      tree
    } else {
      super.atPos(pos)(tree)
    }
  }

  // ---------------- Validating positions ----------------------------------

  override def validatePositions(tree: Tree) {
    def reportTree(prefix : String, tree : Tree) {
      val source = if (tree.pos.isDefined) tree.pos.source else ""
      inform("== "+prefix+" tree ["+tree.id+"] of type "+tree.productPrefix+" at "+tree.pos.show+source)
      inform("")
      inform(treeStatus(tree))
      inform("")
    }

    def positionError(msg: String)(body : => Unit) {
      inform("======= Position error\n" + msg)
      body
      inform("\nWhile validating #" + tree.id)
      inform(treeStatus(tree))
      inform("\nChildren:")
      tree.children map (t => "  " + treeStatus(t, tree)) foreach inform
      inform("=======")
      throw new ValidateException(msg)
    }

    def validate(tree: Tree, encltree: Tree): Unit = {

      if (!tree.isEmpty && tree.canHaveAttrs) {
        if (settings.Yposdebug.value && (settings.verbose.value || settings.Yrangepos.value))
          println("[%10s] %s".format("validate", treeStatus(tree, encltree)))

        if (!tree.pos.isDefined)
          positionError("Unpositioned tree #"+tree.id) {
            inform("%15s %s".format("unpositioned", treeStatus(tree, encltree)))
            inform("%15s %s".format("enclosing", treeStatus(encltree)))
            encltree.children foreach (t => inform("%15s %s".format("sibling", treeStatus(t, encltree))))
          }
        if (tree.pos.isRange) {
          if (!encltree.pos.isRange)
            positionError("Synthetic tree ["+encltree.id+"] contains nonsynthetic tree ["+tree.id+"]") {
            reportTree("Enclosing", encltree)
            reportTree("Enclosed", tree)
            }
          if (!(encltree.pos includes tree.pos))
            positionError("Enclosing tree ["+encltree.id+"] does not include tree ["+tree.id+"]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }

          findOverlapping(tree.children flatMap solidDescendants) match {
            case List() => ;
            case xs => {
              positionError("Overlapping trees "+xs.map { case (x, y) => (x.id, y.id) }.mkString("", ", ", "")) {
                reportTree("Ancestor", tree)
                for((x, y) <- xs) {
                  reportTree("First overlapping", x)
                  reportTree("Second overlapping", y)
                }
              }
            }
          }
        }
        for (ct <- tree.children flatMap solidDescendants) validate(ct, tree)
      }
    }

    if (phase.id <= currentRun.typerPhase.id)
      validate(tree, tree)
  }

  class ValidateException(msg : String) extends Exception(msg)

  // ---------------- Locating trees ----------------------------------

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
    override def traverse(t: Tree) {
      t match {
        case tt : TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
          traverse(tt.original)
        case _ =>
          if (t.pos includes pos) {
            if (isEligible(t)) last = t
            super.traverse(t)
          } else t match {
            case mdef: MemberDef =>
              traverseTrees(mdef.mods.annotations)
            case _ =>
          }
      }
    }
  }

  class TypedLocator(pos: Position) extends Locator(pos) {
    override protected def isEligible(t: Tree) = super.isEligible(t) && t.tpe != null
  }
}
