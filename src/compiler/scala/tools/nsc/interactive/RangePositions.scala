package scala.tools.nsc
package interactive

import ast.Trees
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, OffsetPosition, NoPosition, SyntheticOffsetPosition, WorkScheduler}
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

  class TransparentPosition(source0: SourceFile, start: Int, point: Int, end: Int) extends RangePosition(source0, start, point, end) {
    override def show = "<"+super.show+">"
  }

  def isTransparent(pos: Position) = pos.isInstanceOf[TransparentPosition]

  def isRange(pos: Position) = pos.isInstanceOf[RangePosition] && !isTransparent(pos)

  protected var splitAllowed = false

  override def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new RangePosition(source, start, point, end)

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  override def wrappingPos(trees: List[Tree]): Position = {
    val headpos = trees.head.pos
    if (headpos.isDefined) {
      val source = headpos.source.get
      val point = headpos.point
      val nonsynthetic = trees filter (t => t.pos.isDefined && !t.pos.isSynthetic)
      if (nonsynthetic.isEmpty)
        new SyntheticOffsetPosition(source, point)
      else
        new RangePosition(source, (nonsynthetic map (_.pos.start)).min, point, (nonsynthetic map (_.pos.end)).max)
    } else {
      headpos
    }
  }

  override def makeTransparent(pos: Position) = pos match {
    case rp: RangePosition if (!rp.isSynthetic) =>
      new TransparentPosition(rp.source.get, rp.start, rp.point, rp.end)
    case _ =>
      pos
  }

  // -------------- ensuring no overlaps -------------------------------

  def solidDescendants(tree: Tree): List[Tree] =
    if (isTransparent(tree.pos)) tree.children flatMap solidDescendants
    else List(tree)

  /** A free range from `lo` to `hi` */
  private def free(lo: Int, hi: Int): Range =
    Range(new RangePosition(null, lo, lo, hi), EmptyTree)

  /** The maximal free range */
  private lazy val maxFree: Range = free(0, Math.MAX_INT)

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
      assert(!isTransparent(t.pos))
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

  /** Ensure that given list of trees has mutually non-overlapping positions,
   *  by assinging TransparentPositions to some of them, if necessary
   */
  def ensureNonOverlapping(cts: List[Tree]): Unit = {

    def isSplittable(node: Tree) = node match {
      case Function(_, _) | CaseDef(_, _, _) | Match(_, _) => true
      case _ => false
    }

    /** Do a pass over all child trees `cts`, where `ranges` reflects positions previously
     *  encountered. If there are overlaps, break up one node by making its position a TransparentPosition
     *  and do another pass of `ensureOverlapping`.
     *  @param ranges   The current list of non-overlapping ranges,
     *                  both occupied and free, sorted from later to earlier.
     *                  No TransparentPositions allowed here!
     *  @param trees    The list of trees to insert in ranges.
     */
    def iterate(ranges: List[Range], trees: List[Tree]): Unit = trees match {
      case List() =>
        ;
      case tree :: trees1 =>
        if (isTransparent(tree.pos))
          iterate(ranges, solidDescendants(tree) ::: trees1)
        else if (!tree.pos.isDefined || tree.pos.isSynthetic)
          iterate(ranges, trees1)
        else {
          val conflicting = new ListBuffer[Tree]
          val ranges1 = insert(ranges, tree, conflicting)
//          println("inserted "+tree+"; ranges = "+ranges1)
          if (conflicting.isEmpty) {
            iterate(ranges1, trees1)
          } else {
            val splitNode =
              if (conflicting.size == 1 && (conflicting.head.pos includes tree.pos)) {
                println("*** splitting \n"+conflicting.head+"\n--- because it conflicts with ---\n"+tree)
                println(tree.id)
                conflicting.head
              } else {
                println("*** splitting \n"+tree+"\n--- because it conflicts with trees in ---\n"+conflicting)
                println(tree.id)
                tree
              }
            //throw new Error()//debug

//          println("splitting "+splitNode)
            splitNode setPos new TransparentPosition(splitNode.pos.source.get, splitNode.pos.start, splitNode.pos.point, splitNode.pos.end)
            ensureNonOverlapping(replace(cts, splitNode, solidDescendants(splitNode)))
          }
        }
    }
//    println("ensure non overlap "+cts)
    if (phase.id <= currentRun.typerPhase.id)
      iterate(List(maxFree), cts)
  }

  /** Does given list of trees have mutually non-overlapping positions? */
  def findOverlapping(cts: List[Tree]): List[(Tree, Tree)] = {
    var ranges = List(maxFree)
    for (ct <- cts) {
      if (ct.pos.isDefined && !ct.pos.isSynthetic) {
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
    var remainingRange = pos
    for (tree <- trees) {
      if (!tree.isEmpty && tree.pos == NoPosition) {
        val children = tree.children filter (c => !c.isEmpty && !c.pos.isSynthetic)
        if (children.isEmpty) {
          tree setPos new OffsetPosition(pos.source.get, remainingRange.start)
        } else {
          setChildrenPos(remainingRange, children)
          tree setPos wrappingPos(children)
        }
        remainingRange = new RangePosition(pos.source.get, tree.pos.end, pos.point, pos.end)
      }
    }
    ensureNonOverlapping(trees)
  } catch {
    case ex: Exception =>
      println("error while set children pos "+pos+" of "+trees)
      throw ex
  }

  /** Position a tree.
   *  This means: Set position of a node and position all its unpositioned children.
   */
  override def atPos[T <: Tree](pos: Position)(tree: T): T =
    if (isRange(pos)) {
      if (!tree.isEmpty && tree.pos == NoPosition) {
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

  // ---------------- Validating positions ----------------------------------

  override def validatePositions(tree: Tree) {
    def reportTree(prefix : String, tree : Tree) {
      val source = tree.pos.source match {
        case Some(sf) => " in file "+sf
        case None => ""
      }

      inform("== "+prefix+" tree ["+tree.id+"] of type "+tree.productPrefix+" at "+tree.pos.show+source)
      inform("")
      inform(tree.toString)
      inform("")
    }

    def error(msg: String)(body : => Unit) {
      inform("======= Bad positions: "+msg)
      inform("")
      body
      inform("=== While validating")
      inform("")
      inform(tree.toString)
      inform("")
      inform("=======")
      throw new ValidateError(msg)
    }

    def validate(tree: Tree, encltree: Tree): Unit = {
      if (!tree.isEmpty) {
        if (!tree.pos.isDefined)
          error("Unpositioned tree ["+tree.id+"]") { reportTree("Unpositioned", tree) }
        if (!tree.pos.isSynthetic) {
          if (encltree.pos.isSynthetic)
            error("Synthetic tree ["+encltree.id+"] contains nonsynthetic tree ["+tree.id+"]") {
            reportTree("Enclosing", encltree)
            reportTree("Enclosed", tree)
            }
          if (!(encltree.pos includes tree.pos))
            error("Enclosing tree ["+encltree.id+"] does not include tree ["+tree.id+"]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }

          findOverlapping(tree.children flatMap solidDescendants) match {
            case List() => ;
            case xs => {
              error("Overlapping trees "+xs.map { case (x, y) => (x.id, y.id) }.mkString("", ", ", "")) {
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

    validate(tree, tree)
  }

  class ValidateError(msg : String) extends Exception(msg)

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
    override def traverse(t: Tree) {
      if (!t.pos.isSynthetic && (t.pos includes pos)) {
        if (!isTransparent(t.pos)) last = t
        super.traverse(t)
      }
    }
  }
}
