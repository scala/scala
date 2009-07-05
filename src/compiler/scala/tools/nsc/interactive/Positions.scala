package scala.tools.nsc.interactive

import ast.Trees
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, OffsetPosition, NoPosition, SyntheticOffsetPosition, WorkScheduler}
import scala.collection.mutable.ListBuffer

trait Positions {
self: nsc.Global =>

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new OffsetPosition(source, point)

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position = trees.head.pos

  def makeTransparent(pos: Position) = pos

  def validatePositions(tree: Tree) {}
}
