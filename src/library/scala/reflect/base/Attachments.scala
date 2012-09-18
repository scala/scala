package scala.reflect
package base

/** Attachments is a generalisation of Position. Typically it stores a Position of a tree, but this can be extended to 
 *  encompass arbitrary payloads.
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

  /** The underlying payload. */
  def all: Set[Any] = Set.empty

  private def matchesTag[T: ClassTag](datum: Any) =
    classTag[T].runtimeClass == datum.getClass

  /** An underlying payload of the given class type `T`. */
  def get[T: ClassTag]: Option[T] =
    (all filter matchesTag[T]).headOption.asInstanceOf[Option[T]]

  /** Creates a copy of this attachment with a new payload added */
  def update[T: ClassTag](attachment: T): Attachments { type Pos = self.Pos } =
    new NonemptyAttachments(this.pos, remove[T].all + attachment)

  /** Creates a copy of this attachment with all payloads of the given class type `T` removed. */
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


