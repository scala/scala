package strawman
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import scala.{Any, Boolean, NoSuchElementException, Nothing, UnsupportedOperationException, Int}
import mutable.{Builder, ListBuffer}


/** Concrete collection type: List */
sealed trait List[+A]
  extends Seq[A]
     with LinearSeq[A]
     with SeqOps[A, List, List[A]]
     with Buildable[A, List[A]] {

  protected[this] def fromIterable[B](c: collection.Iterable[B]): List[B] = List.fromIterable(c)
  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): List[A] = fromIterable(coll)

  protected[this] def newBuilder = List.newBuilder[A]

  @tailrec final def length: Int = if (isEmpty) 0 else tail.length

  /** Prepend element */
  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

  /** Prepend operation that avoids copying this list */
  def ++:[B >: A](prefix: List[B]): List[B] =
    if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ++: this

  /** When concatenating with another list `xs`, avoid copying `xs` */
  override def concat[B >: A](xs: IterableOnce[B]): List[B] = xs match {
    case xs: List[B] => this ++: xs
    case _ => super.concat(xs)
  }

  override def className = "List"
}

case class :: [+A](x: A, private[collection] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  override def isEmpty: Boolean = false
  override def nonEmpty: Boolean = true
  override def head: A = x
  override def tail: List[A] = next
}

case object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def nonEmpty: Boolean = false
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")
}

object List extends IterableFactory[List] {

  def fromIterable[B](coll: collection.Iterable[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.fromIterable(coll).toList
  }

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A].mapResult(_.toList)

  def empty[A <: Any]: List[A] = Nil

}
