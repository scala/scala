package scala
package reflect
package internal

import util._
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
trait Positions extends api.Positions { self: SymbolTable =>
  type Position = scala.reflect.internal.util.Position
  val NoPosition = scala.reflect.internal.util.NoPosition
  implicit val PositionTag = ClassTag[Position](classOf[Position])

  def useOffsetPositions: Boolean = true

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position that is either focused or not.
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position = wrappingPos(default, trees, focus = true)
  def wrappingPos(default: Position, trees: List[Tree], focus: Boolean): Position = {
    if (useOffsetPositions) default else {
      val ranged = trees filter (_.pos.isRange)
      if (ranged.isEmpty) if (focus) default.focus else default
      else Position.range(default.source, (ranged map (_.pos.start)).min, default.point, (ranged map (_.pos.end)).max)
    }
  }

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If some of the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
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
  def ensureNonOverlapping(tree: Tree, others: List[Tree]){ ensureNonOverlapping(tree, others, focus = true) }
  def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {
    if (useOffsetPositions) return

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

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int): Position =
    if (useOffsetPositions) Position.offset(source, point)
    else Position.range(source, start, point, end)

  def validatePositions(tree: Tree) {
    if (useOffsetPositions) return

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
      tree.children foreach (t => inform("  " + treeStatus(t, tree)))
      inform("=======")
      throw new ValidateException(msg)
    }

    def validate(tree: Tree, encltree: Tree): Unit = {

      if (!tree.isEmpty && tree.canHaveAttrs) {
        if (settings.Yposdebug && (settings.verbose || settings.Yrangepos))
          inform("[%10s] %s".format("validate", treeStatus(tree, encltree)))

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

    if (!isPastTyper)
      validate(tree, tree)
  }

  def solidDescendants(tree: Tree): List[Tree] =
    if (tree.pos.isTransparent) tree.children flatMap solidDescendants
    else List(tree)

  /** A free range from `lo` to `hi` */
  private def free(lo: Int, hi: Int): Range =
    Range(Position.range(null, lo, lo, hi), EmptyTree)

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
//      inform("subdividing "+r+"/"+t.pos)
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

  /** Set position of all children of a node
   *  @param  pos   A target position.
   *                Uses the point of the position as the point of all positions it assigns.
   *                Uses the start of this position as an Offset position for unpositioned trees
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
      inform("error while set children pos "+pos+" of "+trees)
      throw ex
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
              val annTrees = mdef.mods.annotations match {
                case Nil if mdef.symbol != null =>
                  // After typechecking, annotations are moved from the modifiers
                  // to the annotation on the symbol of the anotatee.
                  mdef.symbol.annotations.map(_.original)
                case anns => anns
              }
              traverseTrees(annTrees)
            case _ =>
          }
      }
    }
  }

  case class Range(pos: Position, tree: Tree) {
    def isFree = tree == EmptyTree
  }

  class TypedLocator(pos: Position) extends Locator(pos) {
    override protected def isEligible(t: Tree) = super.isEligible(t) && t.tpe != null
  }

  trait PosAssigner extends Traverser {
    var pos: Position
  }
  protected[this] lazy val posAssigner: PosAssigner = new DefaultPosAssigner

  protected class DefaultPosAssigner extends PosAssigner {
    var pos: Position = _
    override def traverse(t: Tree) {
      if (!t.canHaveAttrs) ()
      else if (t.pos == NoPosition) {
        t.setPos(pos)
        super.traverse(t)   // TODO: bug? shouldn't the traverse be outside of the if?
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
  def atPos[T <: Tree](pos: Position)(tree: T): T = {
    if (useOffsetPositions || !pos.isOpaqueRange) {
      posAssigner.pos = pos
      posAssigner.traverse(tree)
      tree
    }
    else {
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        tree.setPos(pos)
        val children = tree.children
        if (children.nonEmpty) {
          if (children.tail.isEmpty) atPos(pos)(children.head)
          else setChildrenPos(pos, children)
        }
      }
      tree
    }
  }
}
