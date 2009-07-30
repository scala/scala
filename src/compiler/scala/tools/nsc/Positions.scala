package scala.tools.nsc

import ast.Trees
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, OffsetPosition, NoPosition, WorkScheduler}
import scala.collection.mutable.ListBuffer

trait Positions {
self: scala.tools.nsc.Global =>

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new OffsetPosition(source, point)

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position = trees.head.pos

  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range or assinging TransparentPositions
   *  to some of the nodes in `tree`.
   */
  def ensureNonOverlapping(tree: Tree, others: List[Tree]) {}

  def validatePositions(tree: Tree) {}
}
