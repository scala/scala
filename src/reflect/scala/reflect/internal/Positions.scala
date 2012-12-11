package scala.reflect
package internal

trait Positions extends api.Positions { self: SymbolTable =>

  type Position = scala.reflect.internal.util.Position
  val NoPosition = scala.reflect.internal.util.NoPosition
  implicit val PositionTag = ClassTag[Position](classOf[Position])

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position that is either focused or not.
   */
  def wrappingPos(default: Position, trees: List[Tree]) = wrappingPos(default, trees, true)
  def wrappingPos(default: Position, trees: List[Tree], focus: Boolean): Position = default

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If some of the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position = trees.head.pos

  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range, assigning TransparentPositions
   *  to some of the nodes in `tree` or focusing on the position.
   */
  def ensureNonOverlapping(tree: Tree, others: List[Tree]){ ensureNonOverlapping(tree, others, true) }
  def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {}

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

  def atPos[T <: Tree](pos: Position)(tree: T): T = {
    posAssigner.pos = pos
    posAssigner.traverse(tree)
    tree
  }
}