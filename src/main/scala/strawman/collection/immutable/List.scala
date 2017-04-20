package strawman.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.{Any, Nothing}
import scala.Predef.???
import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce, LinearSeq, SeqLike, MonoBuildable, PolyBuildable}
import strawman.collection.mutable.{Builder, ListBuffer}


/** Concrete collection type: List */
sealed trait List[+A]
  extends Seq[A]
     with SeqLike[A, List]
     with LinearSeq[A]
     with MonoBuildable[A, List[A]]
     with PolyBuildable[A, List] {

  def fromIterable[B](c: collection.Iterable[B]): List[B] = List.fromIterable(c)

  protected[this] def newBuilderWithSameElemType = List.newBuilder
  def newBuilder[E] = List.newBuilder

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
  override def isEmpty = false
  override def nonEmpty = true
  override def head = x
  override def tail = next
}

case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def nonEmpty = false
  override def head = ???
  override def tail = ???
}

object List extends IterableFactory[List] {

  def fromIterable[B](coll: collection.Iterable[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.fromIterable(coll).toList
  }

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A].mapResult(_.toList)

  def empty[A <: Any]: List[A] = Nil

}
