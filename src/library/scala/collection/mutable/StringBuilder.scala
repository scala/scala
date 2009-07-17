/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: StringBuilder.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.mutable

import collection.generic._
import scala.runtime.RichString
import compat.Platform.arraycopy

/** <p>
 *    A mutable sequence of characters.  This class provides an API compatible
 *    with <a class="java/lang/StringBuilder" href="" target="_top">
 *    <code>java.lang.StringBuilder</code></a>.
 *  </p>generic/
 *
 *  @author Stephane Micheloud
 *  @author Martin Odersky
 *  @version 2.8
 */
@serializable
@SerialVersionUID(0 - 8525408645367278351L)
final class StringBuilder(initCapacity: Int, private val initValue: String)
      extends Builder[Char, String]
         with Vector[Char] {

  require(initCapacity > 0)

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

 /** Sets the length of the character sequence.
   *
   *  @param  newLength  the new length
   *  @throws IndexOutOfBoundsException  if the <code>n</code> argument is negative.
   */
  def setLength(n: Int) {
    require(n >= 0, n)
    while (count < n) append('\0')
    count = n
  }

  /** Returns the current capacity. The capacity is the amount of storage
   *  available for newly inserted characters, beyond which an allocation
   *  will occur.
   *
   *  @return  the current capacity
   */
  def capacity: Int = array.length

  /** Same as <code>ensureCapacity</code>. */
  @deprecated("use `ensureCapacity' instead. An assignment is misleading because\n"+
              "it can never decrease the capacity.")
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
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2
      val newar = new Array[Char](newsize)
      arraycopy(array, 0, newar, 0, count)
      array = newar
    }
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
      throw new StringIndexOutOfBoundsException(index)
    array(index)
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
      throw new StringIndexOutOfBoundsException(index)
    arraycopy(array, index + 1, array, index, count - index - 1)
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
      throw new StringIndexOutOfBoundsException(index)
    array(index) = ch
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
      throw new StringIndexOutOfBoundsException(start)
    if (end > count)
      throw new StringIndexOutOfBoundsException(end)
    if (start > end)
      throw new StringIndexOutOfBoundsException(end - start)
    new String(array, start, end - start)
  }

  def subSequence(start: Int, end: Int): java.lang.CharSequence = substring(start, end)

  /* Appends the string representation of the <code>Any</code> argument.
   */
  def +=(x: Char): this.type = { append(x); this }

  def +(x: Char): this.type = { +=(x); this }


  /** <p>
   *    Appends the string representation of the <code>Any</code>
   *    argument.
   *  </p>
   *  <p>
   *    The argument is converted to a string as if by the method
   *    <code>String.valueOf</code>, and the characters of that
   *    string are then appended to this sequence.
   *  </p>
   *
   *  @param  x   an <code>Any</code> object.
   *  @return     a reference to this object.
   */
  def append(x: Any): StringBuilder =
    append(String.valueOf(x))

  /** Appends the specified string to this character sequence.
   *
   *  @param  s   a string.
   *  @return     a reference to this object.
   */
  def append(s: String): StringBuilder = {
    val str = if (s == null) "null" else s
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
  def appendAll(x: Seq[Char]): StringBuilder =
    appendAll(x.toArray, 0, x.length)

  @deprecated("use appendAll instead. This method is deprecated because of the\n"+
              "possible confusion with `append(Any)'.")
  def append(x: Seq[Char]): StringBuilder =
    appendAll(x)

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
  def appendAll(x: Array[Char]): StringBuilder =
    appendAll(x, 0, x.length)

  @deprecated("use appendAll instead. This method is deprecated because\n"+
              "of the possible confusion with `append(Any)'.")
  def append(x: Array[Char]): StringBuilder =
    appendAll(x)

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
  def appendAll(x: Array[Char], offset: Int, len: Int): StringBuilder = {
    ensureCapacity(count + len)
    arraycopy(x, offset, array, count, len)
    count += len
    this
  }

  @deprecated("use appendAll instead. This method is deprecated because\n"+
              "of the possible confusion with `append(Any, Int, Int)'.")
  def append(x: Array[Char], offset: Int, len: Int): StringBuilder =
    appendAll(x, offset, len)

  /** <p>
   *    Appends the string representation of the <code>Boolean</code>
   *    argument to the sequence.
   *  </p>
   *  <p>
   *    The argument is converted to a string as if by the method
   *    <code>String.valueOf</code>, and the characters of that
   *   string are then appended to this sequence.
   *  </p>
   *
   *   @param  x  a <code>Boolean</code>.
   *   @return    a reference to this object.
   */
  def append(x: Boolean): StringBuilder = append(String.valueOf(x))
  def append(x: Byte): StringBuilder = append(String.valueOf(x))

  def append(x: Char): StringBuilder = {
    ensureCapacity(count + 1)
    array(count) = x
    count += 1
    this
  }

  def append(x: Short): StringBuilder =
    append(String.valueOf(x))

  def append(x: Int): StringBuilder =
    append(String.valueOf(x))

  def append(x: Long): StringBuilder =
    append(String.valueOf(x))

  def append(x: Float): StringBuilder =
    append(String.valueOf(x))

  def append(x: Double): StringBuilder =
    append(String.valueOf(x))

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
      throw new StringIndexOutOfBoundsException(start)
    val end0 = if (end > count) count else end
    val len = end0 - start
    if (len > 0) {
      arraycopy(array, start + len, array, start, count - end0)
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

  @deprecated("use insertAll instead. This method is deprecated because of the\n"+
              "possible confusion with `insert(Int, Any, Int, Int)'.")
  def insert(index: Int, str: Array[Char], offset: Int, len: Int): StringBuilder =
    insertAll(index, str, offset, len)

  /** <p>
   *    Inserts the string representation of the <code>Any</code>
   *    argument into this character sequence.
   *  </p>
   *  <p>
   *    The second argument is converted to a string as if by the method
   *    <code>String.valueOf</code>, and the characters of that
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
    insert(at, String.valueOf(x))

  /** Inserts the string into this character sequence.
   *
   *  @param  at  the offset position.
   *  @param  x   a string.
   *  @return     a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insert(at: Int, x: String): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException(at)
    val str = if (x == null) "null" else x
    val len = str.length
    ensureCapacity(count + len)
    arraycopy(array, at, array, at + len, count - at)
    str.getChars(0, len, array, at)
    count += len
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
  def insertAll(at: Int, x: Seq[Char]): StringBuilder =
    insertAll(at, x.toArray)

  @deprecated("use insertAll instead. This method is deprecated because of\n"+
              "the possible confusion with `insert(Int, Any)'.")
  def insert(at: Int, x: Seq[Char]): StringBuilder =
    insertAll(at, x)

  /** Inserts the string representation of the <code>Char</code> array
   *  argument into this sequence.
   *
   *  @param  at  the offset position.
   *  @param  x   a character array.
   *  @return     a reference to this object.
   *  @throws StringIndexOutOfBoundsException  if the offset is invalid.
   */
  def insertAll(at: Int, x: Array[Char]): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException(at)
    val len = x.length
    ensureCapacity(count + len)
    arraycopy(array, at, array, at + len, count - at)
    arraycopy(x, 0, array, at, len)
    count += len
    this
  }

  @deprecated("use insertAll instead. This method is deprecated because of\n"+
              "the possible confusion with `insert(Int, Any)'.")
  def insert(at: Int, x: Array[Char]): StringBuilder =
    insertAll(at, x)

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
    insert(at, String.valueOf(x))

  /** <p>
   *    Inserts the string representation of the <code>Byte</code> argument
   *    into this sequence.
   *  </p>
   *  <p>
   *    The offset argument must be greater than or equal to 0, and less than
   *    or equal to the length of this sequence.
   *  </p>
   *
   *  @param  at  the offset position.
   *  @param  x   a <code>Byte</code> value.
   *  @return     a reference to this object.
   */
  def insert(at: Int, x: Byte): StringBuilder =
    insert(at, String.valueOf(x))

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
      throw new StringIndexOutOfBoundsException(at)
    ensureCapacity(count + 1)
    arraycopy(array, at, array, at + 1, count - at)
    array(at) = x
    count += 1
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
    insert(at, String.valueOf(x))

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
    insert(at, String.valueOf(x))

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
    insert(at, String.valueOf(x))

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
    insert(at, String.valueOf(x))

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
    insert(at, String.valueOf(x))

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
  def indexOf(str: String): Int = indexOfSeq(str.toArray)

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
  def indexOf(str: String, fromIndex: Int): Int = indexOfSeq(str.toArray, fromIndex)

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
  def lastIndexOf(str: String): Int = lastIndexOfSeq(str.toArray, count)

  /** <p>
   *    Returns the index within this string of the last occurrence of the
   *    specified substring. The integer returned is the largest value
   *    <code>k</code> such that:
   *  </p><pre>val
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
  def lastIndexOf(str: String, fromIndex: Int): Int = lastIndexOfSeq(str.toArray, fromIndex)

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
  override def reverse(): StringBuilder = {
    var hasSurrogate = false
    val n = count - 1
    var j = (n-1) >> 1
    while (j >= 0) {
      val temp = array(j)
      val temp2 = array(n - j)
      if (!hasSurrogate)
        hasSurrogate =
          (temp >= StringBuilder.MIN_SURROGATE && temp <= StringBuilder.MAX_SURROGATE) ||
       	  (temp2 >= StringBuilder.MIN_SURROGATE && temp2 <= StringBuilder.MAX_SURROGATE)
      array(j) = temp2
      array(n - j) = temp
      j -= 1
    }
    if (hasSurrogate) {
      // Reverse back all valid surrogate pairs
      var i = 0
      while (i < count - 1) {
        val c2 = array(i)
	if (StringBuilder.isLowSurrogate(c2)) {
          val c1 = array(i + 1)
          if (StringBuilder.isHighSurrogate(c1)) {
            array(i) = c1; i += 1
            array(i) = c2
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
  override def toString: String = new String(array, 0, count)

  def result(): String = toString
}


object StringBuilder {

  type Array[T] = scala.Array[T] // !!!

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
    arraycopy(src, 0, dest, 0, Math.min(src.length, newLength))
    dest
  }
}
