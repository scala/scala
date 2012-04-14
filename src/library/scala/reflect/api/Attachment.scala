package scala.reflect
package api

/** Attachment is a generalisation of Position.
 *  Typically it stores a Position of a tree, but this can be extended to encompass arbitrary payloads.
 *
 *  Attachments have to carry positions, because we don't want to introduce even a single additional field in Tree
 *  imposing an unnecessary memory tax because of something that will not be used in most cases.
 */
// [Eugene] with the introduction of `attach` and `payload[T]` users don't need to create custom attachments anymore
// however, we cannot move attachments to scala.reflect.internal, because they are used in Trees, which are implemented completely in scala.reflect.api
trait Attachment {
  /** Gets the underlying position */
  def pos: Position

  /** Creates a copy of this attachment with its position updated */
  def withPos(newPos: Position): Attachment

  /** Gets the underlying payload */
  def payload: Any

  /** Creates a copy of this attachment with its payload updated */
  def withPayload(newPayload: Any): Attachment
}
