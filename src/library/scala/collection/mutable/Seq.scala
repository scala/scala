package scala.collection.mutable

import scala.collection.SeqFactory
import scala.language.higherKinds

trait Seq[A]
  extends Iterable[A]
    with collection.Seq[A]
    with SeqOps[A, Seq, Seq[A]] {

  override def iterableFactory: SeqFactory[IterableCC] = Seq
}

/**
  * $factoryInfo
  * @define coll mutable sequence
  * @define Coll `mutable.Seq`
  */
@SerialVersionUID(3L)
object Seq extends SeqFactory.Delegate[Seq](ArrayBuffer)

/**
  * @define coll mutable sequence
  * @define Coll `mutable.Seq`
  */
trait SeqOps[A, +CC[_], +C <: AnyRef]
  extends IterableOps[A, CC, C]
    with collection.SeqOps[A, CC, C]
    with Cloneable[C] {

  override def clone(): C = {
    val b = newSpecificBuilder
    b ++= toIterable
    b.result()
  }

  /** Replaces element at given index with a new value.
    *
    *  @param idx      the index of the element to replace.
    *  @param elem     the new value.
    *  @throws   IndexOutOfBoundsException if the index is not valid.
    */
  @throws[IndexOutOfBoundsException]
  def update(idx: Int, elem: A): Unit

  /** Sorts this $coll in place according to an Ordering.
    *
    * @see [[scala.collection.SeqOps.sorted]]
    *
    * @param  ord the ordering to be used to compare elements.
    * @return modified input $coll sorted according to the ordering `ord`.
    */
  def sortInPlace[B >: A](implicit ord: Ordering[B]): this.type = {
    val len = this.length
    if (len > 1) {
      val arr = new Array[AnyRef](len)
      var i = 0
      for (x <- this) {
        arr(i) = x.asInstanceOf[AnyRef]
        i += 1
      }
      java.util.Arrays.sort(arr, ord.asInstanceOf[Ordering[Object]])
      i = 0
      while (i < arr.length) {
        update(i, arr(i).asInstanceOf[A])
        i += 1
      }
    }
    this
  }

  /** Sorts this $coll in place according to a comparison function.
    *
    * @see [[scala.collection.SeqOps.sortWith]]
    */
  def sortInPlaceWith(lt: (A, A) => Boolean): this.type = sortInPlace(Ordering.fromLessThan(lt))

  /** Sorts this $coll in place according to the Ordering which results from transforming
    * an implicitly given Ordering with a transformation function.
    *
    * @see [[scala.collection.SeqOps.sortBy]]
    */
  def sortInPlaceBy[B](f: A => B)(implicit ord: Ordering[B]): this.type = sortInPlace(ord on f)
}

trait IndexedOptimizedSeq[A] extends Seq[A] {

  def mapInPlace(f: A => A): this.type = {
    var i = 0
    val siz = size
    while (i < siz) { this(i) = f(this(i)); i += 1 }
    this
  }
}

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeq[A] extends scala.collection.AbstractSeq[A] with Seq[A]
