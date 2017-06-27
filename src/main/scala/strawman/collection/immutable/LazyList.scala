package strawman
package collection
package immutable

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, Int, None, Nothing, Option, Some, StringContext}
import scala.annotation.tailrec

class LazyList[+A](expr: => LazyList.Evaluated[A])
  extends Seq[A]
     with LinearSeq[A]
     with SeqOps[A, LazyList, LazyList[A]] {

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
  override def tail: LazyList[A] = force.get._2

  @tailrec final def length: Int = if (isEmpty) 0 else tail.length

  def #:: [B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))

  def iterableFactory = LazyList

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): LazyList[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, LazyList[A]] =
    IndexedSeq.newBuilder().mapResult(_.to(LazyList))

// The following is commented because we need to compare it with the default implementation first
//  def zipWithIndex: LazyList[(A, Int)] =
//    LazyList.unfold((0, this)) { case (i, as) =>
//      as.force.map { case (a, _as) => ((a, i), (i + 1, _as)) }
//    }

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

  def empty[A]: LazyList[A] = Empty

  /**
    * @return a LazyList by using a function `f` producing elements of
    *         type `A` and updating an internal state `S`.
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    */
  def unfold[A, S](init: S)(f: S => Option[(A, S)]): LazyList[A] = {
    def loop(s: S): LazyList[A] = new LazyList[A]({
      f(s).map { case (a, _s) => (a, loop(_s)) }
    })
    loop(init)
  }

  def newBuilder[A](): Builder[A, LazyList[A]] = ArrayBuffer.newBuilder[A]().mapResult(fromIterable)

}
