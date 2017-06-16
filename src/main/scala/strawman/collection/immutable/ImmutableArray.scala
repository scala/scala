package strawman.collection.immutable

import strawman.collection.mutable.ArrayBuffer
import strawman.collection.{IterableFactory, IterableOnce, Iterator, View}

import scala.{Any, Boolean, Int, Nothing}
import scala.runtime.ScalaRunTime
import scala.Predef.{???, intWrapper}

/**
  * An immutable array.
  *
  * Supports efficient indexed access and has a small memory footprint.
  */
class ImmutableArray[+A] private (private val elements: scala.Array[Any]) extends IndexedSeq[A] with SeqOps[A, ImmutableArray, ImmutableArray[A]] {

  def iterableFactory: IterableFactory[ImmutableArray] = ImmutableArray

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): ImmutableArray[A] = fromIterable(coll)

  def length: Int = elements.length

  override def knownSize: Int = elements.length

  def apply(i: Int): A = ScalaRunTime.array_apply(elements, i).asInstanceOf[A]

  def iterator(): Iterator[A] = view.iterator()

  override def map[B](f: A => B): ImmutableArray[B] = ImmutableArray.tabulate(length)(i => f(apply(i)))

  override def flatMap[B](f: A => IterableOnce[B]): ImmutableArray[B] = ImmutableArray.fromIterable(View.FlatMap(coll, f))

  def :+ [B >: A](elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length + 1)
    java.lang.System.arraycopy(elements, 0, dest, 0, length)
    dest(length) = elem.asInstanceOf[Any]
    new ImmutableArray(dest)
  }

  override def concat[B >: A](xs: IterableOnce[B]): ImmutableArray[B] =
    xs match {
      case bs: ImmutableArray[B] =>
        val dest = scala.Array.ofDim[Any](length + bs.length)
        java.lang.System.arraycopy(elements, 0, dest, 0, length)
        java.lang.System.arraycopy(bs.elements, 0, dest, length, bs.length)
        new ImmutableArray(dest)
      case _ =>
        ImmutableArray.fromIterable(View.Concat(coll, xs))
    }

  override def zip[B](xs: IterableOnce[B]): ImmutableArray[(A, B)] =
    xs match {
      case bs: ImmutableArray[B] =>
        ImmutableArray.tabulate(length min bs.length) { i =>
          (apply(i), bs(i))
        }
      case _ =>
        ImmutableArray.fromIterable(View.Zip(coll, xs))
    }

  override def filter(p: A => Boolean): ImmutableArray[A] = ImmutableArray.fromIterable(View.Filter(coll, p))

  override def partition(p: A => Boolean): (ImmutableArray[A], ImmutableArray[A]) = {
    val pn = View.Partition(coll, p)
    (ImmutableArray.fromIterable(pn.left), ImmutableArray.fromIterable(pn.right))
  }

  override def take(n: Int): ImmutableArray[A] = ImmutableArray.tabulate(n)(apply)

  override def takeRight(n: Int): ImmutableArray[A] = ImmutableArray.tabulate(n min length)(i => apply(length - (n min length) + i))

  override def drop(n: Int): ImmutableArray[A] = ImmutableArray.tabulate((length - n) max 0)(i => apply(n + i))

  override def dropRight(n: Int): ImmutableArray[A] = ImmutableArray.tabulate((length - n) max 0)(apply)

  override def tail: ImmutableArray[A] =
    if (length > 0) {
      val dest = scala.Array.ofDim[Any](length - 1)
      java.lang.System.arraycopy(elements, 1, dest, 0, length - 1)
      new ImmutableArray(dest)
    } else ???

  override def reverse: ImmutableArray[A] = ImmutableArray.tabulate(length)(i => apply(length - 1 - i))

}

object ImmutableArray extends IterableFactory[ImmutableArray] {
  private[this] lazy val emptyImpl = new ImmutableArray[Nothing](new scala.Array[Any](0))

  def empty[A]: ImmutableArray[A] = emptyImpl

  def fromIterable[A](it: strawman.collection.Iterable[A]): ImmutableArray[A] =
    new ImmutableArray(ArrayBuffer.fromIterable(it).asInstanceOf[ArrayBuffer[Any]].toArray)

  override def fill[A](n: Int)(elem: => A): ImmutableArray[A] = tabulate(n)(_ => elem)

  def tabulate[A](n: Int)(f: Int => A): ImmutableArray[A] = {
    val elements = scala.Array.ofDim[Any](n)
    var i = 0
    while (i < n) {
      ScalaRunTime.array_update(elements, i, f(i))
      i = i + 1
    }
    new ImmutableArray(elements)
  }

}
