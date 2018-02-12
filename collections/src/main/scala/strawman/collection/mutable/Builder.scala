package strawman.collection.mutable

import scala.{Boolean, Any, Char, Int, Unit, Array, Byte, Float, Double, Long, Short, `inline`, deprecated, Serializable}
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
    def addOne(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    def result(): NewTo = f(self.result())
  }

}

class StringBuilder(private val sb: java.lang.StringBuilder) extends Builder[Char, String]
  with IndexedSeq[Char]
  with IndexedOptimizedSeq[Char]
  with Serializable {

  def this() = this(new java.lang.StringBuilder)

  def this(length: Int) = this(new java.lang.StringBuilder(length))

  def this(s: String) = this(new java.lang.StringBuilder(s))

  // Methods required to make this an IndexedSeq:
  def apply(i: Int): Char = sb.charAt(i)
  def iterableFactory: strawman.collection.SeqFactory[IndexedSeq] = IndexedSeq
  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[Char]): IndexedSeq[Char] =
    iterableFactory.from(coll)
  protected[this] def newSpecificBuilder(): strawman.collection.mutable.Builder[Char, IndexedSeq[Char]] =
    iterableFactory.newBuilder()

  //TODO In the old collections, StringBuilder extends Seq -- should it do the same here to get this method?
  def length: Int = sb.length()

  def addOne(x: Char) = { sb.append(x); this }

  def clear() = sb.setLength(0)

  /** Overloaded version of `addAll` that takes a string */
  def addAll(s: String): this.type = { sb.append(s); this }

  /** Alias for `addAll` */
  def ++= (s: String): this.type = addAll(s)

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

  /** Remove a subsequence of Chars from this sequence, starting at the
    *  given start index (inclusive) and extending to the end index (exclusive)
    *  or to the end of the String, whichever comes first.
    *
    *  @param  start  The beginning index, inclusive.
    *  @param  end    The ending index, exclusive.
    *  @return        This StringBuilder.
    *  @throws StringIndexOutOfBoundsException   if start < 0 || start > end
    */
  def delete(start: Int, end: Int): StringBuilder = {
    sb.delete(start, end)
    this
  }

  /** Replaces a subsequence of Chars with the given String.  The semantics
    *  are as in delete, with the String argument then inserted at index 'start'.
    *
    *  @param  start  The beginning index, inclusive.
    *  @param  end    The ending index, exclusive.
    *  @param  str    The String to be inserted at the start index.
    *  @return        This StringBuilder.
    *  @throws StringIndexOutOfBoundsException if start < 0, start > length, or start > end
    */
  def replace(start: Int, end: Int, str: String): StringBuilder = {
    sb.replace(start, end, str)
    this
  }

  /** Inserts a subarray of the given Array[Char] at the given index
    *  of this sequence.
    *
    * @param  index   index at which to insert the subarray.
    * @param  str     the Array from which Chars will be taken.
    * @param  offset  the index of the first Char to insert.
    * @param  len     the number of Chars from 'str' to insert.
    * @return         This StringBuilder.
    *
    * @throws StringIndexOutOfBoundsException  if index < 0, index > length,
    *         offset < 0, len < 0, or (offset + len) > str.length.
    */
  def insertAll(index: Int, str: Array[Char], offset: Int, len: Int): StringBuilder = {
    sb.insert(index, str, offset, len)
    this
  }

  /** Inserts the String representation (via String.valueOf) of the given
    *  argument into this sequence at the given index.
    *
    *  @param  index   the index at which to insert.
    *  @param  x       a value.
    *  @return         this StringBuilder.
    *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
    */
  def insert(index: Int, x: Any): StringBuilder = insert(index, String.valueOf(x))

  /** Inserts the String into this character sequence.
    *
    *  @param  index the index at which to insert.
    *  @param  x     a String.
    *  @return       this StringBuilder.
    *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
    */
  def insert(index: Int, x: String): StringBuilder = {
    sb.insert(index, x)
    this
  }

  /** Inserts the given Seq[Char] into this sequence at the given index.
    *
    *  @param  index the index at which to insert.
    *  @param  xs    the Seq[Char].
    *  @return       this StringBuilder.
    *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
    */
  def insertAll(index: Int, xs: IterableOnce[Char]): StringBuilder =
    insertAll(index, (ArrayBuilder.make[Char]() ++= xs).result())

  /** Inserts the given Array[Char] into this sequence at the given index.
    *
    *  @param  index the index at which to insert.
    *  @param  xs    the Array[Char].
    *  @return       this StringBuilder.
    *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
    */
  def insertAll(index: Int, xs: Array[Char]): StringBuilder = {
    sb.insert(index, xs)
    this
  }

  /** Calls String.valueOf on the given primitive value, and inserts the
    *  String at the given index.
    *
    *  @param  index the offset position.
    *  @param  x     a primitive value.
    *  @return       this StringBuilder.
    */
  def insert(index: Int, x: Boolean): StringBuilder = insert(index, String.valueOf(x))
  def insert(index: Int, x: Byte): StringBuilder    = insert(index, x.toInt)
  def insert(index: Int, x: Short): StringBuilder   = insert(index, x.toInt)
  def insert(index: Int, x: Int): StringBuilder     = insert(index, String.valueOf(x))
  def insert(index: Int, x: Long): StringBuilder    = insert(index, String.valueOf(x))
  def insert(index: Int, x: Float): StringBuilder   = insert(index, String.valueOf(x))
  def insert(index: Int, x: Double): StringBuilder  = insert(index, String.valueOf(x))
  def insert(index: Int, x: Char): StringBuilder    = insert(index, String.valueOf(x))

  /** Sets the length of the character sequence.  If the current sequence
    *  is shorter than the given length, it is padded with nulls; if it is
    *  longer, it is truncated.
    *
    *  @param  len  the new length
    *  @throws IndexOutOfBoundsException if the argument is negative.
    */
  def setLength(len: Int): Unit = sb.setLength(len)

  def update(idx: Int, elem: Char): Unit = sb.setCharAt(idx, elem)
}
