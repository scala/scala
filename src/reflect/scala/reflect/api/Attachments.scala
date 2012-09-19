package scala.reflect
package api

/** Attachments is a generalization of Position. Typically it stores a Position of a tree, but this can be extended to
 *  encompass arbitrary payloads. Payloads are stored in type-indexed slots, which can be read with `get[T]` and written
 *  with `update[T]` and `remove[T]`.
 *
 *  Attachments always carry positions because we don't want to introduce an additional field for attachments in `Tree`
 *  imposing an unnecessary memory tax because of something that will not be used in most cases.
 */
abstract class Attachments { self =>

  /** The position type of this attachment */
  type Pos >: Null

  /** The underlying position */
  def pos: Pos

  /** Creates a copy of this attachment with the position replaced by `newPos` */
  def withPos(newPos: Pos): Attachments { type Pos = self.Pos }

  /** The underlying payload with the guarantee that no two elements have the same type. */
  def all: Set[Any] = Set.empty

  private def matchesTag[T: ClassTag](datum: Any) =
    classTag[T].runtimeClass == datum.getClass

  /** An underlying payload of the given class type `T`. */
  def get[T: ClassTag]: Option[T] =
    (all filter matchesTag[T]).headOption.asInstanceOf[Option[T]]

  /** Creates a copy of this attachment with the payload slot of T added/updated with the provided value.
   *
   * Replaces an existing payload of the same type, if exists.
   */
  def update[T: ClassTag](attachment: T): Attachments { type Pos = self.Pos } =
    new NonemptyAttachments(this.pos, remove[T].all + attachment)

  /** Creates a copy of this attachment with the payload of the given class type `T` removed. */
  def remove[T: ClassTag]: Attachments { type Pos = self.Pos } = {
    val newAll = all filterNot matchesTag[T]
    if (newAll.isEmpty) pos.asInstanceOf[Attachments { type Pos = self.Pos }]
    else new NonemptyAttachments(this.pos, newAll)
  }

  private class NonemptyAttachments(override val pos: Pos, override val all: Set[Any]) extends Attachments {
    type Pos = self.Pos
    def withPos(newPos: Pos) = new NonemptyAttachments(newPos, all)
  }
}
