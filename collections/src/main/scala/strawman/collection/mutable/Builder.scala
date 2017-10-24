package strawman.collection.mutable

import scala.{Boolean, Any, Char, Int, Unit, Array, Byte, Float, Double, Long, Short, `inline`, deprecated}
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
  final def sizeHint(coll: strawman.collection.Iterable[_], delta: Int = 0): Unit = {
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

  //TODO In the old collections, StringBuilder extends Seq -- should it do the same here to get this method?
  def length: Int = sb.length()

  def add(x: Char) = { sb.append(x); this }

  def clear() = sb.setLength(0)

  /** Overloaded version of `addAllInPlace` that takes a string */
  def addAllInPlace(s: String): this.type = { sb.append(s); this }

  /** Alias for `addAllInPlace` */
  def ++= (s: String): this.type = addAllInPlace(s)

  def result() = sb.toString

  override def toString = result()

  // append* methods delegate to the underlying java.lang.StringBuilder:

  def appendAll(xs: String): StringBuilder = {
    sb append xs
    this
  }

  /** Appends the string representation of the given argument,
    *  which is converted to a String with `String.valueOf`.
    *
    *  @param  x   an `Any` object.
    *  @return     this StringBuilder.
    */
  def append(x: Any): StringBuilder = {
    sb append String.valueOf(x)
    this
  }

  /** Appends the given String to this sequence.
    *
    *  @param  s   a String.
    *  @return     this StringBuilder.
    */
  def append(s: String): StringBuilder = {
    sb append s
    this
  }

  /** Appends the specified string builder to this sequence.
    *
    *  @param s
    *  @return
    */
  def append(s: StringBuilder): StringBuilder = {
    sb append s
    this
  }

  /*
  /** Appends all the Chars in the given Seq[Char] to this sequence.
    *
    *  @param  xs  the characters to be appended.
    *  @return     this StringBuilder.
    */
  def appendAll(xs: TraversableOnce[Char]): StringBuilder = appendAll(xs.toArray)
  */

  /** Appends all the Chars in the given Array[Char] to this sequence.
    *
    *  @param  xs  the characters to be appended.
    *  @return     a reference to this object.
    */
  def appendAll(xs: Array[Char]): StringBuilder = {
    sb append xs
    this
  }

  /** Appends a portion of the given Array[Char] to this sequence.
    *
    *  @param  xs      the Array containing Chars to be appended.
    *  @param  offset  the index of the first Char to append.
    *  @param  len     the numbers of Chars to append.
    *  @return         this StringBuilder.
    */
  def appendAll(xs: Array[Char], offset: Int, len: Int): StringBuilder = {
    sb.append(xs, offset, len)
    this
  }

  /** Append the String representation of the given primitive type
    *  to this sequence.  The argument is converted to a String with
    *  String.valueOf.
    *
    *  @param   x  a primitive value
    *  @return     This StringBuilder.
    */
  def append(x: Boolean): StringBuilder = { sb append x ; this }
  def append(x: Byte): StringBuilder = append(x.toInt)
  def append(x: Short): StringBuilder = append(x.toInt)
  def append(x: Int): StringBuilder = { sb append x ; this }
  def append(x: Long): StringBuilder = { sb append x ; this }
  def append(x: Float): StringBuilder = { sb append x ; this }
  def append(x: Double): StringBuilder = { sb append x ; this }
  def append(x: Char): StringBuilder = { sb append x ; this }
}
