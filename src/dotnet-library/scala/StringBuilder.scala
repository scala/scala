/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import Predef._

/** <p>
 *    A mutable sequence of characters.  This class provides an API compatible
 *    with <a class="java/lang/StringBuilder" href="" target="_top">
 *    <code>java.lang.StringBuilder</code></a>.
 *  </p>
 *
 *  @author Stephane Micheloud
 *  @version 1.0
 */
final class StringBuilder(initCapacity: Int, private val initValue: String)
      extends (Int => Char) {
  if (initCapacity < 0) throw new IllegalArgumentException
  if (initValue eq null) throw new NullPointerException

  /** The value is used for character storage. */
  private var value = new Array[Char](initCapacity + initValue.length)

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

  def this(str: String) = this(16, str)

  append(initValue)

  def toArray: Array[Char] = value

  def length: Int = count

  def length_=(n: Int) { setLength(n) }

  /** Sets the length of the character sequence.
   *
   *  @param  newLength  the new length
   *  @throws IndexOutOfBoundsException  if the <code>n</code> argument is negative.
   */
  def setLength(n: Int) {
    if (n < 0)
      throw new StringIndexOutOfBoundsException//(n)
    if (n > value.length) expandCapacity(n)
    if (count < n)
      while (count < n) {
        value(count) = '\0'; count += 1
      }
    else
      count = n
  }

  /** Returns the current capacity. The capacity is the amount of storage
   *  available for newly inserted characters, beyond which an allocation
   *  will occur.
   *
   *  @return  the current capacity
   */
  def capacity: Int = value.length

  /** Same as <code>ensureCapacity</code>. */
  def capacity_=(n: Int) { ensureCapacity(n) }

  /** <p>
   *    Ensures that the capacity is at least equal to the specified minimum.
   *    If the current capacity is less than the argument, then a new internal
   *    array is allocated with greater capacity. The new capacity is the larger of:
   *  </p>
   *  <ul>
   *    <li>The <code>n</code> argument.
   *    <li>Twice the old capacity, plus <code>2</code>.
   *  </ul>
   *  <p>
   *    If the <code>n</code> argument is non-positive, this
   *    method takes no action and simply returns.
   *  </p>
   *
   *  @param n the minimum desired capacity.
   */
  def ensureCapacity(n: Int) {
    if (n > value.length) expandCapacity(n)
  }

  private def expandCapacity(n: Int) {
    val newCapacity = (value.length + 1) * 2
    value = StringBuilder.copyOf(
      value,
      if (newCapacity < 0) Math.MAX_INT else if (n > newCapacity) n else newCapacity
    )
  }

  /** <p>
   *    Returns the <code>Char</code> value in this sequence at the specified index.
   *    The first <code>Char</code> value is at index <code>0</code>, the next at index
   *    <code>1</code>, and so on, as in array indexing.
   *  </p>
   *  <p>
   *    The index argument must be greater than or equal to
   *    <code>0</code>, and less than the length of this sequence.
   *  </p>
   *
   *  @param  index   the index of the desired <code>Char</code> value.
   *  @return         the <code>Char</code> value at the specified index.
   *  @throws IndexOutOfBoundsException  if <code>index</code> is
   *                  negative or greater than or equal to <code>length()</code>.
   */
  def charAt(index: Int): Char = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException//(index)
    value(index)
  }

  /** Same as <code>charAt</code>. */
  def apply(i: Int): Char = charAt(i)

  /** <p>
   *    Removes the <code>Char</code> at the specified position in this
   *    sequence. This sequence is shortened by one <code>Char</code>.
   *  </p>
   *
   *  @param  index  Index of <code>Char</code> to remove
   *  @return        This object.
   *  @throws StringIndexOutOfBoundsException  if the <code>index</code>
   *	             is negative or greater than or equal to <code>length()</code>.
   */
  def deleteCharAt(index: Int): StringBuilder = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException//(index)
    compat.Platform.arraycopy(value, index + 1, value, index, count - index - 1)
    count -= 1
    this
  }

  /** <p>
   *    The character at the specified index is set to <code>ch</code>. This
   *    sequence is altered to represent a new character sequence that is
   *    identical to the old character sequence, except that it contains the
   *    character <code>ch</code> at position <code>index</code>.
   *  </p>
   *  <p>
   *    The index argument must be greater than or equal to
   *    <code>0</code>, and less than the length of this sequence.
   *  </p>
   *
   *  @param  index   the index of the character to modify.
   *  @param  ch      the new character.
   *  @throws IndexOutOfBoundsException  if <code>index</code> is
   *                  negative or greater than or equal to <code>length()</code>.
   */
  def setCharAt(index: Int, ch: Char) {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException//(index)
    value(index) = ch
  }

  /** Same as <code>setCharAt</code>. */
  def update(i: Int, c: Char) { setCharAt(i, c) }

  /** Returns a new <code>String</code> that contains a subsequence of
   *  characters currently contained in this character sequence. The
   *  substring begins at the specified index and extends to the end of
   *  this sequence.
   *
   *  @param  start  The beginning index, inclusive.
   *  @return        The new string.
   *  @throws StringIndexOutOfBoundsException  if <code>start</code> is
   *                 less than zero, or greater than the length of this object.
   */
  def substring(start: Int): String = substring(start, count)

  /** Returns a new <code>String</code> that contains a subsequence of
   *  characters currently contained in this sequence. The
   *  substring begins at the specified <code>start</code> and
   *  extends to the character at index <code>end - 1</code>.
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @return The new string.
   *  @throws StringIndexOutOfBoundsException  if <code>start</code>
   *                 or <code>end</code> are negative or greater than
   *		     <code>length()</code>, or <code>start</code> is
   *		     greater than <code>end</code>.
   */
  def substring(start: Int, end: Int): String = {
    if (start < 0)
      throw new StringIndexOutOfBoundsException//(start)
    if (end > count)
      throw new StringIndexOutOfBoundsException//(end)
    if (start > end)
      throw new StringIndexOutOfBoundsException//(end - start)
    new String(value, start, end - start)
  }

  /** <p>
   *    Appends the string representation of the <code>Any</code>
   *    argument.
   *  </p>
   *  <p>
   *    The argument is converted to a string as if by the method
   *    <code>System.Convert.ToString</code>, and the characters of
   *    that string are then appended to this sequence.
   *  </p>
   *
   *  @param  x   an <code>Any</code> object.
   *  @return     a reference to this object.
   */
  def append(x: Any): StringBuilder =
    append(System.Convert.ToString(x))

  /** Appends the specified string to this character sequence.
   *
   *  @param  s   a string.
   *  @return     a reference to this object.
   */
  def append(s: String): StringBuilder = {
    val str = if (s == null) "null" else s
    val len = str.length
    if (len > 0) {
      val newCount = count + len
      if (newCount > value.length) expandCapacity(newCount)
      compat.Platform.arraycopy(str.ToCharArray, 0, value, count, len)
      count = newCount
    }
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
      val newCount = count + len
      if (newCount > value.length) expandCapacity(newCount)
      compat.Platform.arraycopy(sb.toArray, 0, value, count, len)
      count = newCount
      this
    }

  /** <p>
   *    Appends the string representation of the <code>Char</code> sequence
   *    argument to this sequence.
   *  </p>
   *  <p>
   *    The characters of the sequence argument are appended, in order,
   *    to the contents of this sequence. The length of this sequence
   *    increases by the length of the argument.
   *  </p>
   *
   *  @param  x  the characters to be appended.
   *  @return    a reference to this object.
   */
  def append(x: Seq[Char]): StringBuilder =
    append(x.toArray, 0, x.length)

  /** <p>
   *    Appends the string representation of the <code>Char</code> array
   *    argument to this sequence.
   *  </p>
   *  <p>
   *    The characters of the array argument are appended, in order, to
   *    the contents of this sequence. The length of this sequence
   *    increases by the length of the argument.
   *  </p>
   *
   *  @param  x  the characters to be appended.
   *  @return    a reference to this object.
   */
  def append(x: Array[Char]): StringBuilder =
    append(x, 0, x.length)

  /** <p>
   *    Appends the string representation of a subarray of the
   *    <code>char</code> array argument to this sequence.
   *  </p>
   *  <p>
   *    Characters of the <code>Char</code> array <code>x</code>, starting at
   *    index <code>offset</code>, are appended, in order, to the contents
   *    of this sequence. The length of this sequence increases
   *    by the value of <code>len</code>.
   *  </p>
   *
   *  @param  x      the characters to be appended.
   *  @param  offset the index of the first <code>Char</code> to append.
   *  @param  len    the number of <code>Char</code>s to append.
   *  @return        a reference to this object.
   */
  def append(x: Array[Char], offset: Int, len: Int): StringBuilder = {
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(x, offset, value, count, len)
    count = newCount
    this
  }

  /** <p>
   *    Appends the string representation of the <code>Boolean</code>
   *    argument to the sequence.
   *  </p>
   *  <p>
   *    The argument is converted to a string as if by the method
   *    <code>System.Convert.ToString</code>, and the characters of
   *    that string are then appended to this sequence.
   *  </p>
   *
   *   @param  x  a <code>Boolean</code>.
   *   @return    a reference to this object.
   */
  def append(x: Boolean): StringBuilder = {
    if (x) {
      val newCount = count + 4
      if (newCount > value.length) expandCapacity(newCount)
      value(count) = 't'; count += 1
      value(count) = 'r'; count += 1
      value(count) = 'u'; count += 1
      value(count) = 'e'; count += 1
    } else {
      val newCount = count + 5
      if (newCount > value.length) expandCapacity(newCount)
      value(count) = 'f'; count += 1
      value(count) = 'a'; count += 1
      value(count) = 'l'; count += 1
      value(count) = 's'; count += 1
      value(count) = 'e'; count += 1
    }
    this
  }

  def append(x: Char): StringBuilder = {
    val newCount = count + 1
    if (newCount > value.length) expandCapacity(newCount)
    value(count) = x; count += 1
    this
  }

  def append(x: Short): StringBuilder =
    append(System.Convert.ToString(x))

  def append(x: Int): StringBuilder =
    append(System.Convert.ToString(x))

  def append(x: Long): StringBuilder =
    append(System.Convert.ToString(x))

  def append(x: Float): StringBuilder =
    append(System.Convert.ToString(x))

  def append(x: Double): StringBuilder =
    append(System.Convert.ToString(x))

  /** Removes the characters in a substring of this sequence.
   *  The substring begins at the specified <code>start</code> and extends to
   *  the character at index <code>end - 1</code> or to the end of the
   *  sequence if no such character exists. If
   *  <code>start</code> is equal to <code>end</code>, no changes are made.
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @return        This object.
   *  @throws StringIndexOutOfBoundsException  if <code>start</code>
   *                 is negative, greater than <code>length()</code>, or
   *		     greater than <code>end</code>.
   */
  def delete(start: Int, end: Int): StringBuilder = {
    if (start < 0 || start > end)
      throw new StringIndexOutOfBoundsException//(start)
    val end0 = if (end > count) count else end
    val len = end0 - start
    if (len > 0) {
      compat.Platform.arraycopy(value, start + len, value, start, count - end0)
      count -= len
    }
    this
  }

  /** Replaces the characters in a substring of this sequence
   *  with characters in the specified <code>String</code>. The substring
   *  begins at the specified <code>start</code> and extends to the character
   *  at index <code>end - 1</code> or to the end of the sequence if no such
   *  character exists. First the characters in the substring are removed and
   *  then the specified <code>String</code> is inserted at <code>start</code>.
   *
   *  @param  start  The beginning index, inclusive.
   *  @param  end    The ending index, exclusive.
   *  @param  str    String that will replace previous contents.
   *  @return        This object.
   *  @throws StringIndexOutOfBoundsException  if <code>start</code>
   *                 is negative, greater than <code>length()</code>, or
   *		     greater than <code>end</code>.
   */
  def replace(start: Int, end: Int, str: String) {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException//(start)

    val end0 = if (end > count) count else end
    val len = str.length()
    val newCount = count + len - (end0 - start)
    if (newCount > value.length) expandCapacity(newCount)

    compat.Platform.arraycopy(value, end, value, start + len, count - end)
    compat.Platform.arraycopy(str.ToCharArray, 0, value, start, len)
    count = newCount
    this
  }

  /** Inserts the string representation of a subarray of the <code>str</code>
   *  array argument into this sequence. The subarray begins at the specified
   *  <code>offset</code> and extends <code>len</code> <code>char</code>s.
   *  The characters of the subarray are inserted into this sequence at
   *  the position indicated by <code>index</code>. The length of this
   *  sequence increases by <code>len</code> <code>Char</code>s.
   *
   * @param  index   position at which to insert subarray.
   * @param  str     a <code>Char</code> array.
   * @param  offset  the index of the first <code>char</code> in subarray to
   *                 be inserted.
   * @param  len     the number of <code>Char</code>s in the subarray to
   *                 be inserted.
   * @return         This object
   * @throws StringIndexOutOfBoundsException  if <code>index</code>
   *                 is negative or greater than <code>length()</code>, or
   *                 <code>offset</code> or <code>len</code> are negative, or
   *                 <code>(offset+len)</code> is greater than
   *                 <code>str.length</code>.
   */
  def insert(index: Int, str: Array[Char], offset: Int, len: Int): StringBuilder = {
    if (index < 0 || index > count)
      throw new StringIndexOutOfBoundsException//(index)
    if (offset < 0 || len < 0 || offset > str.length - len)
      throw new StringIndexOutOfBoundsException/*(
                "offset " + offset + ", len " + len +
                ", str.length " + str.length)*/
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, index, value, index + len, count - index)
    compat.Platform.arraycopy(str, offset, value, index, len)
    count = newCount
    this
  }

  /** <p>
   *    Inserts the string representation of the <code>Any</code>
   *    argument into this character sequence.
   *  </p>
   *  <p>
   *    The second argument is converted to a string as if by the method
   *    <code>System.Convert.ToString</code>, and the characters of that
   *    string are then inserted into this sequence at the indicated
   *    offset.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to
   *    <code>0</code>, and less than or equal to the length of this
   *    sequence.
   *  </p>
   *
   *  @param  offset  the offset.
   *  @param  x       an <code>Any</code> value.
   *  @return         a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insert(at: Int, x: Any): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** Inserts the string into this character sequence.
   *
   *  @param  at  the offset position.
   *  @param  x   a string.
   *  @return     a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insert(at: Int, x: String): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException//(at)
    val str = if (x == null) "null" else x
    val len = str.length
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + len, count - at)
    compat.Platform.arraycopy(str.ToCharArray, 0, value, at, len)
    count = newCount
    this
  }

  /** Inserts the string representation of the <code>Char</code> sequence
   *  argument into this sequence.
   *
   *  @param  at  the offset position.
   *  @param  x   a character sequence.
   *  @return     a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insert(at: Int, x: Seq[Char]): StringBuilder =
    insert(at, x.toArray)

  /** Inserts the string representation of the <code>Char</code> array
   *  argument into this sequence.
   *
   *  @param  at  the offset position.
   *  @param  x   a character array.
   *  @return     a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insert(at: Int, x: Array[Char]): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException//(at)
    val len = x.length
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + len, count - at)
    compat.Platform.arraycopy(x, 0, value, at, len)
    count = newCount
    this
  }

  /** <p>
   *    Inserts the string representation of the <code>Boolean</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Boolean</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Boolean): StringBuilder =
    insert(at, System.Convert.ToString(x))


  /** <p>
   *    Inserts the string representation of the <code>Char</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Char</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Char): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException//(at)
    val newCount = count + 1
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + 1, count - at)
    value(at) = x
    count = newCount
    this
  }

  /** <p>
   *    Inserts the string representation of the <code>Short</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Short</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Short): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** <p>
   *    Inserts the string representation of the <code>Int</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Int</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Int): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** <p>
   *    Inserts the string representation of the <code>Long</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Long</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Long): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** <p>
   *    Inserts the string representation of the <code>Float</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Float</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Float): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** <p>
   *    Inserts the string representation of the <code>Double</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Double</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Double): StringBuilder =
    insert(at, System.Convert.ToString(x))

  /** <p>
   *    Returns the index within this string of the first occurrence of the
   *    specified substring. The integer returned is the smallest value
   *    <i>k</i> such that:
   *  </p>
   *  <blockquote><pre>
   *  this.toString().startsWith(str, <i>k</i>)</pre>
   *  </blockquote>
   *  <p>
   *    is <code>true</code>.
   *  </p>
   *
   *  @param  str  any string.
   *  @return      if the string argument occurs as a substring within this
   *               object, then the index of the first character of the first
   *               such substring is returned; if it does not occur as a
   *               substring, <code>-1</code> is returned.
   *  @throws NullPointerException if <code>str</code> is <code>null</code>.
   */
  def indexOf(str: String): Int = indexOf(str, 0)

  /** <p>
   *    Returns the index within this string of the first occurrence of the
   *    specified substring, starting at the specified index. The integer
   *    returned is the smallest value <code>k</code> for which:
   *  </p><pre>
   *    k >= Math.min(fromIndex, str.length()) &&
   *                   this.toString().startsWith(str, k)</pre>
   *  <p>
   *    If no such value of <code>k</code> exists, then <code>-1</code>
   *    is returned.
   *  </p>
   *
   *  @param str        the substring for which to search.
   *  @param fromIndex  the index from which to start the search.
   *  @return           the index within this string of the first occurrence
   *                    of the specified substring, starting at the specified index.
   */
  def indexOf(str: String, fromIndex: Int): Int =
    StringBuilder.indexOf(value, 0, count, str.ToCharArray, 0, str.length(), fromIndex)

  /** <p>
   *    Returns the index within this string of the rightmost occurrence
   *    of the specified substring.  The rightmost empty string "" is
   *    considered to occur at the index value <code>this.length()</code>.
   *    The returned index is the largest value <i>k</i> such that
   *  </p>
   *  <blockquote><pre>
   *  this.toString().startsWith(str, k)</pre>
   *  </blockquote>
   *  <p>
   *    is true.
   *  </p>
   *
   * @param  str  the substring to search for.
   * @return      if the string argument occurs one or more times as a substring
   *              within this object, then the index of the first character of
   *              the last such substring is returned. If it does not occur as
   *              a substring, <code>-1</code> is returned.
   * @throws NullPointerException  if <code>str</code> is <code>null</code>.
   */
  def lastIndexOf(str: String): Int = lastIndexOf(str, count)

  /** <p>
   *    Returns the index within this string of the last occurrence of the
   *    specified substring. The integer returned is the largest value
   *    <code>k</code> such that:
   *  </p><pre>
   *    k <= Math.min(fromIndex, str.length()) &&
   *                   this.toString().startsWith(str, k)</pre>
   *  <p>
   *    If no such value of <code>k</code> exists, then <code>-1</code>
   *    is returned.
   *  </p>
   *
   *  @param  str        the substring to search for.
   *  @param  fromIndex  the index to start the search from.
   *  @return            the index within this sequence of the last occurrence
   *                     of the specified substring.
   */
  def lastIndexOf(str: String, fromIndex: Int): Int =
    StringBuilder.lastIndexOf(value, 0, count, str.ToCharArray, 0, str.length(), fromIndex)

  /** <p>
   *    Causes this character sequence to be replaced by the reverse of the
   *    sequence. If there are any surrogate pairs included in the sequence,
   *    these are treated as single characters for the reverse operation.
   *    Thus, the order of the high-low surrogates is never reversed.
   *  </p>
   *  <p>
   *    Let <i>n</i> be the character length of this character sequence
   *    (not the length in <code>Char</code> values) just prior to
   *    execution of the <code>reverse</code> method. Then the
   *    character at index <i>k</i> in the new character sequence is
   *    equal to the character at index <i>n-k-1</i> in the old
   *    character sequence.
   *  </p>
   *
   *  @return  a reference to this object.
   */
  def reverse(): StringBuilder = {
    import StringBuilder._
    var hasSurrogate = false
    val n = count - 1
    var j = (n-1) >> 1
    while (j >= 0) {
      val temp = value(j)
      val temp2 = value(n - j)
      if (!hasSurrogate)
        hasSurrogate =
          (temp >= MIN_HIGH_SURROGATE && temp <= MAX_LOW_SURROGATE) ||
       	  (temp2 >= MIN_HIGH_SURROGATE && temp2 <= MAX_LOW_SURROGATE)
      value(j) = temp2
      value(n - j) = temp
      j -= 1
    }
    if (hasSurrogate) {
      // Reverse back all valid surrogate pairs
      var i = 0
      while (i < count - 1) {
        val c2 = value(i)
	if (isLowSurrogate(c2)) {
          val c1 = value(i + 1)
          if (isHighSurrogate(c1)) {
            value(i) = c1; i += 1
            value(i) = c2
          }
        }
        i += 1
      }
    }
    this
  }

  /** Returns a string representing the data in this sequence.
   *  A new <code>String</code> object is allocated and initialized to
   *  contain the character sequence currently represented by this
   *  object. This <code>String</code> is then returned. Subsequent
   *  changes to this sequence do not affect the contents of the
   *  <code>String</code>.
   *
   *  @return  a string representation of this sequence of characters.
   */
  override def toString(): String = new String(value, 0, count)

}


object StringBuilder
{
  private val MIN_HIGH_SURROGATE = '\uD800'
  private val MAX_HIGH_SURROGATE = '\uDBFF'

  private val MIN_LOW_SURROGATE = '\uDC00'
  private val MAX_LOW_SURROGATE = '\uDFFF'

  // constants <code>java.langCharacter.MIN-/MAX_SURROGATE</code> exist since 1.5
  private val MIN_SURROGATE = MIN_HIGH_SURROGATE
  private val MAX_SURROGATE = MAX_LOW_SURROGATE

  // methods <code>java.langCharacter.isLow-/isHighSurrogate</code> exist since 1.5
  private def isLowSurrogate(ch: Char): Boolean =
    MIN_LOW_SURROGATE <= ch && ch <= MAX_LOW_SURROGATE

  private def isHighSurrogate(ch: Char): Boolean =
    MIN_HIGH_SURROGATE <= ch && ch <= MAX_HIGH_SURROGATE

  // method <code>java.util.Arrays.copyOf</code> exists since 1.6
  private def copyOf(src: Array[Char], newLength: Int): Array[Char] = {
    val dest = new Array[Char](newLength)
    compat.Platform.arraycopy(src, 0, dest, 0, Math.min(src.length, newLength))
    dest
  }

  private def indexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int,
                      target: Array[Char], targetOffset: Int, targetCount: Int,
                      fromIndex: Int): Int =
    if (fromIndex >= sourceCount)
      if (targetCount == 0) sourceCount else -1
    else {
      val inx = if (fromIndex < 0) 0 else fromIndex
      if (targetCount == 0)
        inx
      else {
        val first  = target(targetOffset)
        val max = sourceOffset + (sourceCount - targetCount)

        var i = sourceOffset + inx
        while (i <= max) {
          /* Look for first character. */
          if (source(i) != first) {
            i += 1
            while (i <= max && source(i) != first) i += 1
          }
          /* Found first character, now look at the rest of v2 */
          if (i <= max) {
            var j = i + 1
            val end = j + targetCount - 1
            var k = targetOffset + 1
            while (j < end && source(j) == target(k)) {
              j += 1
              k += 1
            }
            if (j == end) {
              /* Found whole string. */
              return i - sourceOffset
            }
          } // if
          i += 1
        } // while
        -1
      }
    }

  private def lastIndexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int,
                          target: Array[Char], targetOffset: Int, targetCount: Int,
                          fromIndex: Int): Int = {
    val rightIndex = sourceCount - targetCount
    if (fromIndex < 0) return -1
    val inx = if (fromIndex > rightIndex) rightIndex else fromIndex
    // Empty string always matches
    if (targetCount == 0) return inx

    val strLastIndex = targetOffset + targetCount - 1
    val strLastChar = target(strLastIndex)
    val min = sourceOffset + targetCount - 1
    var i = min + fromIndex

    while (true) {
      while (i >= min && source(i) != strLastChar) i -= 1
      if (i < min) return -1
      var j = i - 1
      val start = j - (targetCount - 1)
      var k = strLastIndex - 1
      var outerWhile = false
      while (j > start && !outerWhile) {
        if (source(j) != target(k)) {
          j -= 1
          k -= 1
          i -= 1
          outerWhile = true
        }
      }
      if (!outerWhile) return start - sourceOffset + 1
    }
    -1
  }
}
