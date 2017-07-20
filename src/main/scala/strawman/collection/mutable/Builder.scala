package strawman.collection.mutable

import scala.{Boolean, Any, Char, Int, Unit}
import java.lang.String

import strawman.collection.IterableOnce

import scala.math.Ordering

/** Base trait for collection builders */
trait Builder[-A, +To] extends Growable[A] { self =>

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear(): Unit

  /** Result collection consisting of all elements appended so far. */
  def result(): To

  /** Gives a hint how many elements are expected to be added
    *  when the next `result` is called. Some builder classes
    *  will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param size  the hint how many elements will be added.
    */
  def sizeHint(size: Int): Unit = ()

  /** Gives a hint that one expects the `result` of this builder
    *  to have the same size as the given collection, plus some delta. This will
    *  provide a hint only if the collection has a known size
    *  Some builder classes
    *  will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param coll  the collection which serves as a hint for the result's size.
    *  @param delta a correction to add to the `coll.size` to produce the size hint.
    */
  final def sizeHint(coll: strawman.collection.Iterable[_], delta: Int): Unit = {
    if (coll.knownSize != -1) {
      sizeHint(coll.knownSize + delta)
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
  final def sizeHintBounded(size: Int, boundingColl: strawman.collection.Iterable[_]): Unit = {
    if (boundingColl.knownSize != -1) {
      sizeHint(scala.math.min(boundingColl.knownSize, size))
    }
  }

  /** A builder resulting from this builder my mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo) = new Builder[A, NewTo] {
    def add(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    def result(): NewTo = f(self.result())
  }

}

class StringBuilder extends Builder[Char, String] {
  private val sb = new java.lang.StringBuilder

  def add(x: Char) = { sb.append(x); this }

  def clear() = sb.setLength(0)

  /** Overloaded version of `addAllInPlace` that takes a string */
  def addAllInPlace(s: String): this.type = { sb.append(s); this }

  /** Alias for `addAllInPlace` */
  def ++= (s: String): this.type = addAllInPlace(s)

  def result() = sb.toString

  override def toString = result()
}
