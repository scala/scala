package strawman
package collection.immutable

import strawman.collection.mutable.{ArrayBuffer, Builder, GrowableBuilder}
import strawman.collection.{IterableFactory, IterableOnce, Iterator, StrictOptimizedIterableOps, View}

import scala.{Any, Boolean, Int, Nothing}
import scala.runtime.ScalaRunTime
import scala.Predef.{???, intWrapper}

/**
  * An immutable array.
  *
  * Supports efficient indexed access and has a small memory footprint.
  */
class ImmutableArray[+A] private[collection] (private val elements: scala.Array[Any])
  extends IndexedSeq[A]
    with IndexedSeqOps[A, ImmutableArray, ImmutableArray[A]]
    with StrictOptimizedIterableOps[A, ImmutableArray[A]] {

  def iterableFactory: IterableFactory[ImmutableArray] = ImmutableArray

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): ImmutableArray[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, ImmutableArray[A]] = ImmutableArray.newBuilder[A]()

  def length: Int = elements.length

  override def knownSize: Int = elements.length

  def apply(i: Int): A = ScalaRunTime.array_apply(elements, i).asInstanceOf[A]

  def iterator(): Iterator[A] = view.iterator()

  override def updated[B >: A](index: Int, elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length)
    java.lang.System.arraycopy(elements, 0, dest, 0, length)
    dest(index) = elem
    new ImmutableArray(dest)
  }

  override def map[B](f: A => B): ImmutableArray[B] = ImmutableArray.tabulate(length)(i => f(apply(i)))

  override def prepend[B >: A](elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length + 1)
    dest(0) = elem
    java.lang.System.arraycopy(elements, 0, dest, 1, length)
    new ImmutableArray(dest)
  }

  override def append [B >: A](elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length + 1)
    java.lang.System.arraycopy(elements, 0, dest, 0, length)
    dest(length) = elem
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
    (ImmutableArray.fromIterable(pn.first), ImmutableArray.fromIterable(pn.second))
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

  def fromArrayBuffer[A](arr: ArrayBuffer[A]): ImmutableArray[A] =
    new ImmutableArray[A](arr.asInstanceOf[ArrayBuffer[Any]].toArray)

  def fromIterable[A](it: strawman.collection.Iterable[A]): ImmutableArray[A] =
    fromArrayBuffer(ArrayBuffer.fromIterable(it))

  def newBuilder[A](): Builder[A, ImmutableArray[A]] =
    ArrayBuffer.newBuilder[A]().mapResult(fromArrayBuffer)

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
