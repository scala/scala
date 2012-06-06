package scala.reflect
package base

/** Attachments is a generalisation of Position.
 *  Typically it stores a Position of a tree, but this can be extended to encompass arbitrary payloads.
 *
 *  Attachments have to carry positions, because we don't want to introduce even a single additional field in Tree
 *  imposing an unnecessary memory tax because of something that will not be used in most cases.
 */
abstract class Attachments { self =>

  type Pos >: Null

  /** Gets the underlying position */
  def pos: Pos

  /** Creates a copy of this attachment with its position updated */
  def withPos(newPos: Pos): Attachments { type Pos = self.Pos }

  /** Gets the underlying payload */
  def all: Set[Any] = Set.empty

  def get[T: ClassTag]: Option[T] =
    (all find (_.getClass == classTag[T].runtimeClass)).asInstanceOf[Option[T]]

  /** Creates a copy of this attachment with its payload updated */
  def add(attachment: Any): Attachments { type Pos = self.Pos } =
    new NonemptyAttachments(this.pos, all + attachment)

  def remove[T: ClassTag]: Attachments { type Pos = self.Pos } = {
    val newAll = all filterNot (_.getClass == classTag[T].runtimeClass)
    if (newAll.isEmpty) pos.asInstanceOf[Attachments { type Pos = self.Pos }]
    else new NonemptyAttachments(this.pos, newAll)
  }

  private class NonemptyAttachments(override val pos: Pos, override val all: Set[Any]) extends Attachments {
    type Pos = self.Pos
    def withPos(newPos: Pos) = new NonemptyAttachments(newPos, all)
  }
}


