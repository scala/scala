package scala.reflect
package api

/**
 * Defines the type hierachy for positions.
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Positions {
  self: Universe =>

  /** .. */
  type Position >: Null <: scala.reflect.api.Position { type Pos = Position }

  /** A tag that preserves the identity of the `Position` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val PositionTag: ClassTag[Position]

  /** A special "missing" position. */
  val NoPosition: Position

  /** Assigns a given position to all position-less nodes of a given AST.
   */
  def atPos[T <: Tree](pos: Position)(tree: T): T

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position.
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position
}
