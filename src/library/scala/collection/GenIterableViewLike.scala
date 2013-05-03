/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

trait GenIterableViewLike[+A,
                          +Coll,
                          +This <: GenIterableView[A, Coll] with GenIterableViewLike[A, Coll, This]]
extends GenIterable[A] with GenIterableLike[A, This] with GenTraversableView[A, Coll] with GenTraversableViewLike[A, Coll, This] {
self =>

  trait Transformed[+B] extends GenIterableView[B, Coll] with super.Transformed[B] {
    def iterator: Iterator[B]
    override def foreach[U](f: B => U): Unit = iterator foreach f
    override def toString = viewToString
    override def isEmpty = !iterator.hasNext
  }

  trait EmptyView extends Transformed[Nothing] with super.EmptyView {
    final def iterator: Iterator[Nothing] = Iterator.empty
  }

  trait Forced[B] extends super.Forced[B] with Transformed[B] {
    def iterator = forced.iterator
  }

  trait Sliced extends super.Sliced with Transformed[A] {
    def iterator: Iterator[A] = self.iterator.slice(from, until)
  }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B] {
    def iterator = self.iterator map mapping
  }

  trait FlatMapped[B] extends super.FlatMapped[B] with Transformed[B] {
    def iterator: Iterator[B] = self.iterator flatMap mapping
  }

  trait Appended[B >: A] extends super.Appended[B] with Transformed[B] {
    def iterator = self.iterator ++ rest
  }

  trait Filtered extends super.Filtered with Transformed[A] {
    def iterator = self.iterator filter pred
  }

  trait TakenWhile extends super.TakenWhile with Transformed[A] {
    def iterator = self.iterator takeWhile pred
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[A] {
    def iterator = self.iterator dropWhile pred
  }

  trait Zipped[B] extends Transformed[(A, B)] {
    protected[this] val other: GenIterable[B]
    def iterator: Iterator[(A, B)] = self.iterator zip other.iterator
    final override protected[this] def viewIdentifier = "Z"
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] {
    protected[this] val other: GenIterable[B]
    protected[this] val thisElem: A1
    protected[this] val thatElem: B
    final override protected[this] def viewIdentifier = "Z"
    def iterator: Iterator[(A1, B)] =
      self.iterator.zipAll(other.iterator, thisElem, thatElem)
  }

}


