package scala.reflect
package api

/** Attachment is a generalisation of Position.
 *  Typically it stores a Position of a tree, but this can be extended to encompass arbitrary payloads.
 *
 *  Attachments have to carry positions, because we don't want to introduce even a single additional field in Tree
 *  imposing an unnecessary memory tax because of something that will not be used in most cases.
 */
trait Attachment {
  /** Gets the underlying position */
  def pos: Position

  /** Creates a copy of this attachment with its position updated */
  def withPos(pos: Position): Attachment
}
