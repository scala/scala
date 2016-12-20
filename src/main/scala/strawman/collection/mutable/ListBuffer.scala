package strawman.collection.mutable

import scala.{Int, Unit}
import strawman.collection.{SeqLike, IterableFactory, Iterable, Seq}
import strawman.collection.immutable.{List, Nil, ::}

/** Concrete collection type: ListBuffer */
class ListBuffer[A]
  extends Seq[A]
    with SeqLike[A, ListBuffer]
    with Buildable[A, ListBuffer[A]]
    with Builder[A, ListBuffer[A]] {

  private var first, last: List[A] = Nil
  private var aliased = false
  private var len = 0

  def iterator() = first.iterator()

  def fromIterable[B](coll: Iterable[B]) = ListBuffer.fromIterable(coll)

  def apply(i: Int) = first.apply(i)

  def length = len
  override def knownSize = len

  protected[this] def newBuilder = new ListBuffer[A]

  private def copyElems(): Unit = {
    val buf = ListBuffer.fromIterable(result)
    first = buf.first
    last = buf.last
    aliased = false
  }

  /** Convert to list; avoids copying where possible. */
  def toList = {
    aliased = true
    first
  }

  def +=(elem: A) = {
    if (aliased) copyElems()
    val last1 = elem :: Nil
    last match {
      case last: ::[A] => last.next = last1
      case _ => first = last1
    }
    last = last1
    len += 1
    this
  }

  def result = this

  override def className = "ListBuffer"
}

object ListBuffer extends IterableFactory[ListBuffer] {
  def fromIterable[B](coll: Iterable[B]): ListBuffer[B] = new ListBuffer[B] ++= coll
}
