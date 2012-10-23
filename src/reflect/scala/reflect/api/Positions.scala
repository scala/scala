package scala.reflect
package api

/** This trait defines the concept of positions and operations on them.
 *
 *  @see [[scala.reflect.api.Position]]
 *
 *  @contentDiagram hideNodes "*Api"
 */
trait Positions {
  self: Universe =>

  /** Defines a universe-specific notion of positions.
   *  The main documentation entry about positions is located at [[scala.reflect.api.Position]].
   *  @group Positions
   */
  type Position >: Null <: scala.reflect.api.Position { type Pos = Position }

  /** A tag that preserves the identity of the `Position` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val PositionTag: ClassTag[Position]

  /** A special "missing" position.
   *  @group Positions
   */
  val NoPosition: Position

  /** Assigns a given position to all position-less nodes of a given AST.
   *  @group Positions
   */
  def atPos[T <: Tree](pos: Position)(tree: T): T

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position.
   *  @group Positions
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   *  @group Positions
   */
  def wrappingPos(trees: List[Tree]): Position
}
