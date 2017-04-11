package strawman.collection.immutable

import scala.{None, Nothing, Option, Some, StringContext, Any}
import scala.Predef.???
import strawman.collection
import strawman.collection.mutable.Builder
import strawman.collection.{IterableFactory, Iterator, LinearSeq, SeqLike}

class LazyList[+A](expr: => LazyList.Evaluated[A])
  extends Seq[A] with SeqLike[A, LazyList] with LinearSeq[A] {
  private[this] var evaluated = false
  private[this] var result: LazyList.Evaluated[A] = _

  def force: LazyList.Evaluated[A] = {
    if (!evaluated) {
      result = expr
      evaluated = true
    }
    result
  }

  override def isEmpty = force.isEmpty
  override def nonEmpty = force.nonEmpty
  override def head = force.get._1
  override def tail = force.get._2

  def #:: [B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))

  def fromIterable[B](c: collection.Iterable[B]): LazyList[B] = LazyList.fromIterable(c)

  override def className = "LazyList"

  override def toString =
    if (evaluated)
      result match {
        case None => "Empty"
        case Some((hd, tl)) => s"$hd #:: $tl"
      }
    else "LazyList(?)"
}

object LazyList extends IterableFactory[LazyList] {

  type Evaluated[+A] = Option[(A, LazyList[A])]

  object Empty extends LazyList[Nothing](None)

  object #:: {
    def unapply[A](s: LazyList[A]): Evaluated[A] = s.force
  }

  def fromIterable[A](coll: collection.Iterable[A]): LazyList[A] = coll match {
    case coll: LazyList[A] => coll
    case _ => fromIterator(coll.iterator())
  }

  def fromIterator[A](it: Iterator[A]): LazyList[A] =
    new LazyList(if (it.hasNext) Some(it.next(), fromIterator(it)) else None)

  def newBuilder[A]: Builder[A, LazyList[A]] = ???

  def empty[A <: Any]: LazyList[A] = new LazyList[A](None)

}
