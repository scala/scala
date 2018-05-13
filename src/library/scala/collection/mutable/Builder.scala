package scala.collection.mutable

import java.lang.String

import scala.collection.IterableOnce

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
  final def sizeHint(coll: scala.collection.IterableOnce[_], delta: Int = 0): Unit = {
    val s = coll.knownSize
    if (s != -1) sizeHint(s + delta)
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
  final def sizeHintBounded(size: Int, boundingColl: scala.collection.Iterable[_]): Unit = {
    if (boundingColl.knownSize != -1) {
      sizeHint(scala.math.min(boundingColl.knownSize, size))
    }
  }

  /** A builder resulting from this builder my mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo): Builder[A, NewTo] = new Builder[A, NewTo] {
    def addOne(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    override def sizeHint(size: Int): Unit = self.sizeHint(size)
    def result(): NewTo = f(self.result())
  }

}

/** A builder for mutable sequence of characters.  This class provides an API
  *  mostly compatible with `java.lang.StringBuilder`, except where there are
  *  conflicts with the Scala collections API (such as the `reverse` method.)
  *
  *  @author Stephane Micheloud
  *  @author Martin Odersky
  *  @version 2.8
  *  @since   2.7
  *  @define Coll `mutable.IndexedSeq`
  *  @define coll string builder
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#stringbuilders "Scala's Collection Library overview"]]
  *  section on `StringBuilders` for more information.
  */
@SerialVersionUID(3L)
class StringBuilder(private val sb: java.lang.StringBuilder) extends AbstractSeq[Char]
  with Builder[Char, String]
  with IndexedSeq[Char]
  with IndexedSeqOps[Char, IndexedSeq, StringBuilder]
  with IndexedOptimizedSeq[Char]
  with java.lang.CharSequence
  with Serializable {

  def this() = this(new java.lang.StringBuilder)

  def this(length: Int) = this(new java.lang.StringBuilder(length))

  def this(s: String) = this(new java.lang.StringBuilder(s))

  // Methods required to make this an IndexedSeq:
  def apply(i: Int): Char = sb.charAt(i)

 override protected def fromSpecificIterable(coll: scala.collection.Iterable[Char]): StringBuilder =
    new StringBuilder() ++= coll

  override protected def newSpecificBuilder: Builder[Char, StringBuilder] =
    new GrowableBuilder(new StringBuilder())

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

  /** Appends all the Chars in the given IterableOnce[Char] to this sequence.
    *
    *  @param  xs  the characters to be appended.
    *  @return     this StringBuilder.
    */
  def appendAll(xs: IterableOnce[Char]): StringBuilder = appendAll(ArrayBuffer.from(xs).toArray)

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
    insertAll(index, (ArrayBuilder.make[Char] ++= xs).result())

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


  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  @deprecated("Use reverseInPlace instead", "2.13.0")
  final def reverseContents(): this.type = reverseInPlace()

  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  def reverseInPlace(): this.type = {
    sb.reverse()
    this
  }


  /** Returns the current capacity, which is the size of the underlying array.
   *  A new array will be allocated if the current capacity is exceeded.
   *
   *  @return  the capacity
   */
  def capacity: Int = sb.capacity()

  /** Ensure that the capacity is at least the given argument.
   *  If the argument is greater than the current capacity, new
   *  storage will be allocated with size equal to the given
   *  argument or to `(2 * capacity + 2)`, whichever is larger.
   *
   *  @param newCapacity    the minimum desired capacity.
   */
  def ensureCapacity(newCapacity: Int): Unit = { sb.ensureCapacity(newCapacity) }

  /** Returns the Char at the specified index, counting from 0 as in Arrays.
   *
   *  @param  index   the index to look up
   *  @return         the Char at the given index.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def charAt(index: Int): Char = sb.charAt(index)

  /** Removes the Char at the specified index.  The sequence is
   *  shortened by one.
   *
   *  @param  index  The index to remove.
   *  @return        This StringBuilder.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def deleteCharAt(index: Int): this.type = {
    sb.deleteCharAt(index)
    this
  }

  /** Update the sequence at the given index to hold the specified Char.
   *
   *  @param  index   the index to modify.
   *  @param  ch      the new Char.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def setCharAt(index: Int, ch: Char): this.type = {
    sb.setCharAt(index, ch)
    this
  }

  /** Returns a new String made up of a subsequence of this sequence,
   *  beginning at the given index and extending to the end of the sequence.
   *
   *  target.substring(start)  is equivalent to  target.drop(start)
   *
   *  @param  start  The starting index, inclusive.
   *  @return        The new String.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def substring(start: Int): String = substring(start, length)

  /** Returns a new String made up of a subsequence of this sequence,
   *  beginning at the start index (inclusive) and extending to the
   *  end index (exclusive).
   *
   *  target.substring(start, end)  is equivalent to  target.slice(start, end).mkString
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @return The new String.
   *  @throws StringIndexOutOfBoundsException If either index is out of bounds,
   *          or if start > end.
   */
  def substring(start: Int, end: Int): String = sb.substring(start, end)

  /** For implementing CharSequence.
   */
  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    substring(start, end)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String): Int = sb.indexOf(str)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String, fromIndex: Int): Int = sb.indexOf(str, fromIndex)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String): Int = sb.lastIndexOf(str)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String, fromIndex: Int): Int = sb.lastIndexOf(str, fromIndex)
}

object StringBuilder {
  @deprecated("Use `new StringBuilder()` instead of `StringBuilder.newBuilder`", "2.13.0")
  def newBuilder = new StringBuilder
}
