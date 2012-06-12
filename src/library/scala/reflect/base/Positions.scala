package scala.reflect
package base

trait Positions {
  self: Universe =>

  /** .. */
  type Position >: Null <: Attachments { type Pos = Position }

  /** A tag that preserves the identity of the `FlagSet` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val PositionTag: ClassTag[Position]

  /** .. */
  val NoPosition: Position

  /** Assigns a given position to all position-less nodes of a given AST.
   */
  def atPos[T <: Tree](pos: Position)(tree: T): T
  // [Eugene++] why do we have this in base?
}
