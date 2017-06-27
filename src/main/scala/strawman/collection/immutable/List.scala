package strawman
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import mutable.{Builder, GrowableBuilder, ListBuffer}
import scala.{Any, Boolean, Int, NoSuchElementException, Nothing, UnsupportedOperationException}


/** Concrete collection type: List */
sealed trait List[+A]
  extends Seq[A]
     with LinearSeq[A]
     with LinearSeqOps[A, List, List[A]]
     with StrictOptimizedIterableOps[A, List[A]] {

  def iterableFactory = List

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): List[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder() = List.newBuilder[A]()

  @tailrec final def length: Int = if (isEmpty) 0 else tail.length

  /** Prepend element */
  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

  override def prepend[B >: A](elem: B): List[B] = elem :: this

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

object List extends IterableFactoryWithBuilder[List] {

  def fromIterable[B](coll: collection.Iterable[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.fromIterable(coll).toList
  }

  def newBuilder[A](): Builder[A, List[A]] = new GrowableBuilder(ListBuffer.empty[A]).mapResult(_.toList)

  def empty[A]: List[A] = Nil
}
