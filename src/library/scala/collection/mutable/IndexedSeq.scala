/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package mutable

trait IndexedSeq[T] extends Seq[T]
  with scala.collection.IndexedSeq[T]
  with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]]
  with IterableFactoryDefaults[T, IndexedSeq] {

  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](ArrayBuffer)

trait IndexedSeqOps[A, +CC[_], +C <: AnyRef]
  extends scala.collection.IndexedSeqOps[A, CC, C]
    with SeqOps[A, CC, C] {

  /** Modifies this $coll by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       this $coll modified by replacing all elements with the
    *                result of applying the given function `f` to each element
    *                of this $coll.
    */
  def mapInPlace(f: A => A): this.type = {
    var i = 0
    val siz = size
    while (i < siz) { this(i) = f(this(i)); i += 1 }
    this
  }

  /** Sorts this $coll in place according to an Ordering.
    *
    * @see [[scala.collection.SeqOps.sorted]]
    * @param  ord the ordering to be used to compare elements.
    * @return modified input $coll sorted according to the ordering `ord`.
    */
  def sortInPlace[B >: A]()(implicit ord: Ordering[B]): this.type = {
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
  def sortInPlaceWith(lt: (A, A) => Boolean): this.type = sortInPlace()(Ordering.fromLessThan(lt))

  /** Sorts this $coll in place according to the Ordering which results from transforming
    * an implicitly given Ordering with a transformation function.
    *
    * @see [[scala.collection.SeqOps.sortBy]]
    */
  def sortInPlaceBy[B](f: A => B)(implicit ord: Ordering[B]): this.type = sortInPlace()(ord on f)

}
