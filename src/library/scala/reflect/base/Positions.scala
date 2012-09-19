package scala.reflect
package base

/**
 * Defines the type hierachy for positions. 
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Positions {
  self: Universe =>

  /** The base type for all positions of tree nodes in source files. */
  type Position >: Null <: Attachments { type Pos = Position }

  /** A tag that preserves the identity of the `Position` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val PositionTag: ClassTag[Position]

  /** A special "missing" position. */
  val NoPosition: Position
}
