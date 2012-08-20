/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package mutable

import generic._

/** The base trait of all builders.
 *  A builder lets one construct a collection incrementally, by adding
 *  elements to the builder with `+=` and then converting to the required
 *  collection type with `result`.
 *
 *  @tparam  Elem  the type of elements that get added to the builder.
 *  @tparam  To    the type of collection that it produced.
 *
 *  @since 2.8
 */
trait Builder[-Elem, +To] extends Growable[Elem] {

  /** Adds a single element to the builder.
   *  @param elem the element to be added.
   *  @return the builder itself.
   */
  def +=(elem: Elem): this.type

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear()

  /** Produces a collection from the added elements.
   *  The builder's contents are undefined after this operation.
   *  @return a collection containing the elements added to this builder.
   */
  def result(): To

  /** Gives a hint how many elements are expected to be added
   *  when the next `result` is called. Some builder classes
   *  will optimize their representation based on the hint. However,
   *  builder implementations are still required to work correctly even if the hint is
   *  wrong, i.e. a different number of elements is added.
   *
   *  @param size  the hint how many elements will be added.
   */
  def sizeHint(size: Int) {}

  /** Gives a hint that one expects the `result` of this builder
   *  to have the same size as the given collection, plus some delta. This will
   *  provide a hint only if the collection is known to have a cheap
   *  `size` method. Currently this is assumed to be the case if and only if
   *  the collection is of type `IndexedSeqLike`.
   *  Some builder classes
   *  will optimize their representation based on the hint. However,
   *  builder implementations are still required to work correctly even if the hint is
   *  wrong, i.e. a different number of elements is added.
   *
   *  @param coll  the collection which serves as a hint for the result's size.
   */
  def sizeHint(coll: TraversableLike[_, _]) {
    if (coll.isInstanceOf[collection.IndexedSeqLike[_,_]]) {
      sizeHint(coll.size)
    }
  }

  /** Gives a hint that one expects the `result` of this builder
   *  to have the same size as the given collection, plus some delta. This will
   *  provide a hint only if the collection is known to have a cheap
   *  `size` method. Currently this is assumed to be the case if and only if
   *  the collection is of type `IndexedSeqLike`.
   *  Some builder classes
   *  will optimize their representation based on the hint. However,
   *  builder implementations are still required to work correctly even if the hint is
   *  wrong, i.e. a different number of elements is added.
   *
   *  @param coll  the collection which serves as a hint for the result's size.
   *  @param delta a correction to add to the `coll.size` to produce the size hint.
   */
  def sizeHint(coll: TraversableLike[_, _], delta: Int) {
    if (coll.isInstanceOf[collection.IndexedSeqLike[_,_]]) {
      sizeHint(coll.size + delta)
    }
  }

  /** Gives a hint how many elements are expected to be added
   *  when the next `result` is called, together with an upper bound
   *  given by the size of some other collection. Some builder classes
   *  will optimize their representation based on the hint. However,
   *  builder implementations are still required to work correctly even if the hint is
   *  wrong, i.e. a different number of elements is added.
   *
   *  @param size  the hint how many elements will be added.
   *  @param boundingColl  the bounding collection. If it is
   *                       an IndexedSeqLike, then sizes larger
   *                       than collection's size are reduced.
   */
  def sizeHintBounded(size: Int, boundingColl: TraversableLike[_, _]) {
    if (boundingColl.isInstanceOf[collection.IndexedSeqLike[_,_]])
      sizeHint(size min boundingColl.size)
  }

  /** Creates a new builder by applying a transformation function to
   *  the results of this builder.
   *  @param  f     the transformation function.
   *  @tparam NewTo the type of collection returned by `f`.
   *  @return a new builder which is the same as the current builder except
   *          that a transformation function is applied to this builder's result.
   */
  def mapResult[NewTo](f: To => NewTo): Builder[Elem, NewTo] =
    new Builder[Elem, NewTo] with Proxy {
      val self = Builder.this
      def +=(x: Elem): this.type = { self += x; this }
      def clear() = self.clear()
      override def ++=(xs: TraversableOnce[Elem]): this.type = { self ++= xs; this }
      override def sizeHint(size: Int) = self.sizeHint(size)
      override def sizeHintBounded(size: Int, boundColl: TraversableLike[_, _]) = self.sizeHintBounded(size, boundColl)
      def result: NewTo = f(self.result)
    }
}

