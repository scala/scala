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

package scala.collection.mutable

/** Base trait for collection builders.
  *
  * After calling `result()` the behavior of a Builder (which is not also a [[scala.collection.mutable.ReusableBuilder]])
  * is undefined. No further methods should be called. It is common for mutable collections to be their own non-reusable
  * Builder, in which case `result()` simply returns `this`.
  *
  * @see [[scala.collection.mutable.ReusableBuilder]] for Builders which can be reused after calling `result()`
  */
trait Builder[-A, +To] extends Growable[A] { self =>

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear(): Unit

  /** Result collection consisting of all elements appended so far. */
  def result(): To

  /** Gives a hint how many elements are expected to be added in total
   *  by the time `result` is called.
   *
   *  Some builder classes will optimize their representation based on the hint.
   *  However, builder implementations are required to work correctly even if the hint is
   *  wrong, e.g., a different number of elements is added, or the hint is out of range.
   *
   *  The default implementation simply ignores the hint.
   *
   *  @param size  the hint how many elements will be added.
   */
  def sizeHint(size: Int): Unit = ()

  /** Gives a hint that the `result` of this builder is expected
   *  to have the same size as the given collection, plus some delta.
   *
   *  This method provides a hint only if the collection has a known size,
   *  as specified by the following pseudocode:
   *
   *  {{{
   *    if (coll.knownSize != -1)
   *      if (coll.knownSize + delta <= 0) sizeHint(0)
   *      else sizeHint(coll.knownSize + delta)
   *  }}}
   *
   *  If the delta is negative and the result size is known to be negative,
   *  then the size hint is issued at zero.
   *
   *  Some builder classes will optimize their representation based on the hint.
   *  However, builder implementations are required to work correctly even if the hint is
   *  wrong, i.e., if a different number of elements is added.
   *
   *  @param coll  the collection which serves as a hint for the result's size.
   *  @param delta a correction to add to the `coll.size` to produce the size hint (zero if omitted).
   */
  final def sizeHint(coll: scala.collection.IterableOnce[_], delta: Int = 0): Unit =
    coll.knownSize match {
      case -1 =>
      case sz => sizeHint(0 max sz + delta)
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
  // should probably be `boundingColl: IterableOnce[_]`, but binary compatibility
  final def sizeHintBounded(size: Int, boundingColl: scala.collection.Iterable[_]): Unit = {
    val s = boundingColl.knownSize
    if (s != -1) {
      sizeHint(scala.math.min(s, size))
    }
  }

  /** A builder resulting from this builder by mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo): Builder[A, NewTo] = new Builder[A, NewTo] {
    def addOne(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    override def sizeHint(size: Int): Unit = self.sizeHint(size)
    def result(): NewTo = f(self.result())
    override def knownSize: Int = self.knownSize
  }
}
