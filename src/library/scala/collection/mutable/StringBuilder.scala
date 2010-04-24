/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import compat.Platform.arraycopy
import scala.reflect.Manifest
import annotation.migration
import StringBuilder._

/** A builder for mutable sequence of characters.  This class provides an API
 *  mostly compatible with java.lang.StringBuilder, except where there are conflicts
 *  with the Scala collections API (such as the `reverse` method.)
 *
 *  @author Stephane Micheloud
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.7
 */
@serializable
@SerialVersionUID(0 - 8525408645367278351L)
final class StringBuilder(initCapacity: Int, private val initValue: String)
      extends Builder[Char, String]
         with IndexedSeq[Char] {

  require(initCapacity >= 0)

  import scala.collection.Seq

  /** The value is used for character storage. */
  private var array = new Array[Char](initCapacity + initValue.length)

  /** The count is the number of characters used. */
  private var count: Int = 0

  /** Constructs a string builder with no characters in it and an
   *  initial capacity of 16 characters.
   */
  def this() = this(16, "")

  /** Constructs a string builder with no characters in it and an
   *  initial capacity specified by the <code>capacity</code> argument.
   *
   *  @param  capacity  the initial capacity.
   *  @throws NegativeArraySizeException  if the <code>capacity</code>
   *                    argument is less than <code>0</code>.
   */
  def this(capacity: Int) = this(capacity, "")

  /** Constructs a string builder with initial characters
   *  equal to characters of `str`.
   */
  def this(str: String) = this(16, str)

  append(initValue)

  def toArray: Array[Char] = array

  def length: Int = count
  def length_=(n: Int) { setLength(n) }

  /** Clears the builder contents.
   */
  def clear(): Unit = setLength(0)

  /** Sets the length of the character sequence.  If the current sequence
   *  is shorter than the given length, it is padded with nulls; if it is
   *  longer, it is truncated.
   *
   *  @param  len  the new length
   *  @throws IndexOutOfBoundsException if the argument is negative.
   */
  def setLength(len: Int) {
    require(len >= 0, len)
    while (count < len) append('\0')
    count = len
  }

  /** Returns the current capacity, which is the size of the underlying array.
   *  A new array will be allocated if the current capacity is exceeded.
   *
   *  @return  the capacity
   */
  def capacity: Int = array.length

  @deprecated("Use `ensureCapacity' instead. An assignment is misleading because\n"+
              "it can never decrease the capacity.")
  def capacity_=(n: Int) { ensureCapacity(n) }

  /** Ensure that the capacity is at least the given argument.
   *  If the argument is greater than the current capacity, new
   *  storage will be allocated with size equal to the given
   *  argument or to (2 * capacity + 2), whichever is larger.
   *
   *  @param newCapacity    the minimum desired capacity.
   */
  def ensureCapacity(newCapacity: Int): Unit =
    if (newCapacity > array.length) {
      val newSize   = (array.length * 2 + 2) max newCapacity
      val newArray  = new Array[Char](newSize)
      arraycopy(array, 0, newArray, 0, count)
      array = newArray
    }

  /** Returns the Char at the specified index, counting from 0 as in Arrays.
   *
   *  @param  index   the index to look up
   *  @return         the Char at the given index.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def charAt(index: Int): Char = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    array(index)
  }

  /** Equivalent to charAt.
   */
  def apply(i: Int): Char = charAt(i)

  /** Removes the Char at the specified index.  The sequence is
   *  shortened by one.
   *
   *  @param  index  The index to remove.
   *  @return        This StringBuilder.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def deleteCharAt(index: Int): StringBuilder = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    arraycopy(array, index + 1, array, index, count - index - 1)
    count -= 1
    this
  }

  /** Update the sequence at the given index to hold the specified Char.
   *
   *  @param  index   the index to modify.
   *  @param  ch      the new Char.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def setCharAt(index: Int, ch: Char) {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    array(index) = ch
  }

  /** Equivalent to setCharAt.
   */
  def update(i: Int, c: Char) { setCharAt(i, c) }

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
  def substring(start: Int, end: Int): String = {
    if (start < 0)
      throw new StringIndexOutOfBoundsException(start)
    if (end > count)
      throw new StringIndexOutOfBoundsException(end)
    if (start > end)
      throw new StringIndexOutOfBoundsException(end - start)
    new String(array, start, end - start)
  }

  def subSequence(start: Int, end: Int): java.lang.CharSequence = substring(start, end)

  /** Appends the given Char to the end of the sequence.
   */
  def +=(x: Char): this.type = { append(x); this }

  /** !!! This should create a new sequence.
   */
  def +(x: Char): this.type = { +=(x); this }

  /** Appends the string representation of the given argument,
   *  which is converted to a String with String.valueOf.
   *
   *  @param  x   an <code>Any</code> object.
   *  @return     this StringBuilder.
   */
  def append(x: Any): StringBuilder = append(String.valueOf(x))

  /** Appends the given String to this sequence.
   *
   *  @param  s   a String.
   *  @return     this StringBuilder.
   */
  def append(s: String): StringBuilder = {
    val str = onull(s)
    val len = str.length
    ensureCapacity(count + len)
    str.getChars(0, len, array, count)
    count += len
    this
  }

  /** Appends the specified string builder to this sequence.
   *
   *  @param sb
   *  @return
   */
  def append(sb: StringBuilder): StringBuilder =
    if (sb == null)
      append("null")
    else {
      val len = sb.length
      ensureCapacity(count + len)
      arraycopy(sb.toArray, 0, array, count, len)
      count += len
      this
    }

  /** Appends all the Chars in the given Seq[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     this StringBuilder.
   */
  def appendAll(xs: Seq[Char]): StringBuilder = appendAll(xs.toArray, 0, xs.length)

  /** Appends all the Chars in the given Array[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     a reference to this object.
   */
  def appendAll(xs: Array[Char]): StringBuilder = appendAll(xs, 0, xs.length)

  /** Appends a portion of the given Array[Char] to this sequence.
   *
   *  @param  xs      the Array containing Chars to be appended.
   *  @param  offset  the index of the first Char to append.
   *  @param  len     the numbers of Chars to append.
   *  @return         this StringBuilder.
   */
  def appendAll(xs: Array[Char], offset: Int, len: Int): StringBuilder = {
    ensureCapacity(count + len)
    arraycopy(xs, offset, array, count, len)
    count += len
    this
  }

  /** Append the String representation of the given primitive type
   *  to this sequence.  The argument is converted to a String with
   *  String.valueOf.
   *
   *  @param   x  a primitive value
   *  @return     This StringBuilder.
   */
  def append(x: Boolean): StringBuilder = append(String.valueOf(x))
  def append(x: Byte): StringBuilder = append(String.valueOf(x))
  def append(x: Short): StringBuilder = append(String.valueOf(x))
  def append(x: Int): StringBuilder = append(String.valueOf(x))
  def append(x: Long): StringBuilder = append(String.valueOf(x))
  def append(x: Float): StringBuilder = append(String.valueOf(x))
  def append(x: Double): StringBuilder = append(String.valueOf(x))
  def append(x: Char): StringBuilder = {
    ensureCapacity(count + 1)
    array(count) = x
    count += 1
    this
  }

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
    if (start < 0 || start > end)
      throw new StringIndexOutOfBoundsException(start)
    val end0 = if (end > count) count else end
    val len = end0 - start
    if (len > 0) {
      arraycopy(array, start + len, array, start, count - end0)
      count -= len
    }
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
  def replace(start: Int, end: Int, str: String) {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start)

    val end0 = if (end > count) count else end
    val len = str.length()
    val newCount = count + len - (end0 - start)
    ensureCapacity(newCount)

    arraycopy(array, end, array, start + len, count - end)
    str.getChars(0, len, array, start)
    count = newCount
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
    if (index < 0 || index > count)
      throw new StringIndexOutOfBoundsException(index)
    if (offset < 0 || len < 0 || offset > str.length - len)
      throw new StringIndexOutOfBoundsException(
                "offset " + offset + ", len " + len +
                ", str.length " + str.length)
    ensureCapacity(count + len)
    arraycopy(array, index, array, index + len, count - index)
    arraycopy(str, offset, array, index, len)
    count += len
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
  def insert(index: Int, x: String): StringBuilder = insertAll(index, x.toArray)

  /** Inserts the given Seq[Char] into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the Seq[Char].
   *  @return       this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: Seq[Char]): StringBuilder = insertAll(index, xs.toArray)

  /** Inserts the given Array[Char] into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the Array[Char].
   *  @return       this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: Array[Char]): StringBuilder = {
    if (index < 0 || index > count)
      throw new StringIndexOutOfBoundsException(index)
    val len = xs.length
    ensureCapacity(count + len)
    arraycopy(array, index, array, index + len, count - index)
    arraycopy(xs, 0, array, index, len)
    count += len
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
  def insert(index: Int, x: Byte): StringBuilder    = insert(index, String.valueOf(x))
  def insert(index: Int, x: Short): StringBuilder   = insert(index, String.valueOf(x))
  def insert(index: Int, x: Int): StringBuilder     = insert(index, String.valueOf(x))
  def insert(index: Int, x: Long): StringBuilder    = insert(index, String.valueOf(x))
  def insert(index: Int, x: Float): StringBuilder   = insert(index, String.valueOf(x))
  def insert(index: Int, x: Double): StringBuilder  = insert(index, String.valueOf(x))
  def insert(index: Int, x: Char): StringBuilder = {
    if (index < 0 || index > count)
      throw new StringIndexOutOfBoundsException(index)
    ensureCapacity(count + 1)
    arraycopy(array, index, array, index + 1, count - index)
    array(index) = x
    count += 1
    this
  }

  @deprecated("Use appendAll instead. This method is deprecated because of the\n"+
              "possible confusion with `append(Any)'.")
  def append(x: Seq[Char]): StringBuilder = appendAll(x)

  @deprecated("use appendAll instead. This method is deprecated because\n"+
              "of the possible confusion with `append(Any)'.")
  def append(x: Array[Char]): StringBuilder = appendAll(x)

  @deprecated("use appendAll instead. This method is deprecated because\n"+
              "of the possible confusion with `append(Any, Int, Int)'.")
  def append(x: Array[Char], offset: Int, len: Int): StringBuilder = appendAll(x, offset, len)

  @deprecated("use insertAll instead. This method is deprecated because of the\n"+
              "possible confusion with `insert(Int, Any, Int, Int)'.")
  def insert(index: Int, str: Array[Char], offset: Int, len: Int): StringBuilder =
    insertAll(index, str, offset, len)

  @deprecated("use insertAll instead. This method is deprecated because of\n"+
              "the possible confusion with `insert(Int, Any)'.")
  def insert(at: Int, x: Seq[Char]): StringBuilder = insertAll(at, x)

  @deprecated("use insertAll instead. This method is deprecated because of\n"+
              "the possible confusion with `insert(Int, Any)'.")
  def insert(at: Int, x: Array[Char]): StringBuilder =
    insertAll(at, x)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String): Int = indexOf(str, 0)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String, fromIndex: Int): Int = indexOfSlice(str.toIndexedSeq, fromIndex)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String): Int = lastIndexOf(str, count)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String, fromIndex: Int): Int = lastIndexOfSlice(str.toIndexedSeq, fromIndex)

  /** Creates a new StringBuilder with the reversed contents of this one.
   *  If surrogate pairs are present, they are treated as indivisible units: each
   *  pair will appear in the same order in the updated sequence.
   *
   *  @return   the reversed StringBuilder
   */
  @migration(2, 8, "Since 2.8 reverse returns a new instance.  Use 'reverseContents' to update in place.")
  override def reverse: StringBuilder = new StringBuilder(this.toString).reverseContents()

  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  def reverseContents(): StringBuilder = {
    // record of indices of pairs which need to be swapped
    val surrogates = new ListBuffer[(Int, Int)]
    val half  = count / 2

    def mirror(x: Int) = count - 1 - x
    def swap(i1: Int, i2: Int) {
      val tmp = array(i2)
      array(i2) = array(i1)
      array(i1) = tmp
    }

    for ((i, j) <- 0 until half zip (count - 1 to half by -1)) {
      if (array(i).isSurrogate && array(i + 1).isSurrogate)
        surrogates += ((j - 1, j))
      if (array(j).isSurrogate && array(j - 1).isSurrogate)
        surrogates += ((i, i + 1))

      swap(i, j)
    }
    surrogates foreach (swap _).tupled
    this
  }

  /** Returns a new String representing the data in this sequence.
   *
   *  @return  the current contents of this sequence as a String
   */
  override def toString: String = new String(array, 0, count)

  def result(): String = toString
}


object StringBuilder
{
  // method <code>java.util.Arrays.copyOf</code> exists since 1.6
  private def copyOf(src: Array[Char], newLength: Int): Array[Char] = {
    val dest = new Array[Char](newLength)
    arraycopy(src, 0, dest, 0, src.length min newLength)
    dest
  }

  // for mimicking java's propensity to make null into "null"
  private def onull(s: String): String = if (s == null) "null" else s
}
