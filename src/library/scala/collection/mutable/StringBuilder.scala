/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import java.lang.{ StringBuilder => JavaStringBuilder }
import scala.annotation.migration
import immutable.StringLike

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
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html# "Scala's Collection Library overview"]]
 *  section on `StringBuilders` for more information.
 */
@SerialVersionUID(0 - 8525408645367278351L)
final class StringBuilder(private val underlying: JavaStringBuilder)
      extends AbstractSeq[Char]
         with java.lang.CharSequence
         with IndexedSeq[Char]
         with StringLike[StringBuilder]
         with ReusableBuilder[Char, String]
         with Serializable {

  override protected[this] def thisCollection: StringBuilder = this
  override protected[this] def toCollection(repr: StringBuilder): StringBuilder = repr

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = new GrowingBuilder(new StringBuilder)

  /** Constructs a string builder initialized with string value `initValue`
   *  and with additional character capacity `initCapacity`.
   */
  def this(initCapacity: Int, initValue: String) =
    this(new JavaStringBuilder(initValue.length + initCapacity) append initValue)

  /** Constructs a string builder with no characters in it and an
   *  initial capacity of 16 characters.
   */
  def this() = this(16, "")

  /** Constructs a string builder with no characters in it and an
   *  initial capacity specified by the `capacity` argument.
   *
   *  @param  capacity  the initial capacity.
   *  @throws NegativeArraySizeException  if capacity < 0.
   */
  def this(capacity: Int) = this(capacity, "")

  /** Constructs a string builder with initial characters
   *  equal to characters of `str`.
   */
  def this(str: String) = this(16, str)

  def toArray: Array[Char] = {
    val arr = new Array[Char](length)
    underlying.getChars(0, length, arr, 0)
    arr
  }

  override def length: Int = underlying.length()
  def length_=(n: Int) { underlying.setLength(n) }

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
  def setLength(len: Int) { underlying setLength len }

  /** Returns the current capacity, which is the size of the underlying array.
   *  A new array will be allocated if the current capacity is exceeded.
   *
   *  @return  the capacity
   */
  def capacity: Int = underlying.capacity()

  /** Ensure that the capacity is at least the given argument.
   *  If the argument is greater than the current capacity, new
   *  storage will be allocated with size equal to the given
   *  argument or to `(2 * capacity + 2)`, whichever is larger.
   *
   *  @param newCapacity    the minimum desired capacity.
   */
  def ensureCapacity(newCapacity: Int) { underlying ensureCapacity newCapacity }

  /** Returns the Char at the specified index, counting from 0 as in Arrays.
   *
   *  @param  index   the index to look up
   *  @return         the Char at the given index.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def charAt(index: Int): Char = underlying charAt index

  /** Equivalent to charAt.
   */
  override def apply(index: Int): Char = underlying charAt index

  /** Removes the Char at the specified index.  The sequence is
   *  shortened by one.
   *
   *  @param  index  The index to remove.
   *  @return        This StringBuilder.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def deleteCharAt(index: Int): StringBuilder = {
    underlying deleteCharAt index
    this
  }

  /** Update the sequence at the given index to hold the specified Char.
   *
   *  @param  index   the index to modify.
   *  @param  ch      the new Char.
   *  @throws IndexOutOfBoundsException  if the index is out of bounds.
   */
  def setCharAt(index: Int, ch: Char): Unit = underlying.setCharAt(index, ch)

  /** Equivalent to setCharAt.
   */
  def update(i: Int, c: Char): Unit = setCharAt(i, c)

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
  def substring(start: Int, end: Int): String = underlying.substring(start, end)

  /** For implementing CharSequence.
   */
  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    substring(start, end)

  /** Appends the given Char to the end of the sequence.
   */
  def +=(x: Char): this.type = { append(x); this }

  /** Optimization.
   */
  def ++=(s: String): this.type = {
    underlying append s
    this
  }

  def appendAll(xs: String): StringBuilder = {
    underlying append xs
    this
  }

  /** !!! This should create a new sequence.
   */
  def +(x: Char): this.type = { +=(x); this }

  /** Appends the string representation of the given argument,
   *  which is converted to a String with `String.valueOf`.
   *
   *  @param  x   an `Any` object.
   *  @return     this StringBuilder.
   */
  def append(x: Any): StringBuilder = {
    underlying append String.valueOf(x)
    this
  }

  /** Appends the given String to this sequence.
   *
   *  @param  s   a String.
   *  @return     this StringBuilder.
   */
  def append(s: String): StringBuilder = {
    underlying append s
    this
  }

  /** Appends the specified string builder to this sequence.
   *
   *  @param sb
   *  @return
   */
  def append(sb: StringBuilder): StringBuilder = {
    underlying append sb
    this
  }

  /** Appends all the Chars in the given Seq[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     this StringBuilder.
   */
  def appendAll(xs: TraversableOnce[Char]): StringBuilder = appendAll(xs.toArray)

  /** Appends all the Chars in the given Array[Char] to this sequence.
   *
   *  @param  xs  the characters to be appended.
   *  @return     a reference to this object.
   */
  def appendAll(xs: Array[Char]): StringBuilder = {
    underlying append xs
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
    underlying.append(xs, offset, len)
    this
  }

  /** Append the String representation of the given primitive type
   *  to this sequence.  The argument is converted to a String with
   *  String.valueOf.
   *
   *  @param   x  a primitive value
   *  @return     This StringBuilder.
   */
  def append(x: Boolean): StringBuilder = { underlying append x ; this }
  def append(x: Byte): StringBuilder = append(x.toInt)
  def append(x: Short): StringBuilder = append(x.toInt)
  def append(x: Int): StringBuilder = { underlying append x ; this }
  def append(x: Long): StringBuilder = { underlying append x ; this }
  def append(x: Float): StringBuilder = { underlying append x ; this }
  def append(x: Double): StringBuilder = { underlying append x ; this }
  def append(x: Char): StringBuilder = { underlying append x ; this }

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
    underlying.delete(start, end)
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
    underlying.replace(start, end, str)
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
    underlying.insert(index, str, offset, len)
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
    underlying.insert(index, x)
    this
  }

  /** Inserts the given Seq[Char] into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the Seq[Char].
   *  @return       this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: TraversableOnce[Char]): StringBuilder = insertAll(index, xs.toArray)

  /** Inserts the given Array[Char] into this sequence at the given index.
   *
   *  @param  index the index at which to insert.
   *  @param  xs    the Array[Char].
   *  @return       this StringBuilder.
   *  @throws StringIndexOutOfBoundsException  if the index is out of bounds.
   */
  def insertAll(index: Int, xs: Array[Char]): StringBuilder = {
    underlying.insert(index, xs)
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

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String): Int = underlying.indexOf(str)

  /** Finds the index of the first occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the first applicable index where target occurs, or -1 if not found.
   */
  def indexOf(str: String, fromIndex: Int): Int = underlying.indexOf(str, fromIndex)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String): Int = underlying.lastIndexOf(str)

  /** Finds the index of the last occurrence of the specified substring.
   *
   *  @param    str       the target string to search for
   *  @param    fromIndex the smallest index in the source string to consider
   *  @return             the last applicable index where target occurs, or -1 if not found.
   */
  def lastIndexOf(str: String, fromIndex: Int): Int = underlying.lastIndexOf(str, fromIndex)

  /** Creates a new StringBuilder with the reversed contents of this one.
   *  If surrogate pairs are present, they are treated as indivisible units: each
   *  pair will appear in the same order in the updated sequence.
   *
   *  @return   the reversed StringBuilder
   */
  @migration("`reverse` returns a new instance.  Use `reverseContents` to update in place and return that StringBuilder itself.", "2.8.0")
  override def reverse: StringBuilder = new StringBuilder(new JavaStringBuilder(underlying).reverse)

  override def clone(): StringBuilder = new StringBuilder(new JavaStringBuilder(underlying))

  /** Like reverse, but destructively updates the target StringBuilder.
   *
   *  @return   the reversed StringBuilder (same as the target StringBuilder)
   */
  def reverseContents(): StringBuilder = {
    underlying.reverse()
    this
  }

  /** Returns a new String representing the data in this sequence.
   *
   *  @note    because toString is inherited from AnyRef and used for
   *           many purposes, it is better practice to call mkString
   *           to obtain a StringBuilder result.
   *  @return  the current contents of this sequence as a String
   */
  override def toString = underlying.toString

  /** Returns a new String representing the data in this sequence.
   *
   *  @return  the current contents of this sequence as a String
   */
  override def mkString = toString

  /** Returns the result of this Builder (a String).
   *
   *  If this method is called multiple times, each call will result in a snapshot of the buffer at that point in time.
   *  In particular, a `StringBuilder` can be used to build multiple independent strings by emptying the buffer with `clear`
   *  after each call to `result`.
   *
   *  @return  the string assembled by this StringBuilder
   */
  def result(): String = toString
}

object StringBuilder {
  def newBuilder = new StringBuilder
}
