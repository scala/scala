/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala

import Predef._

/** <p>
 *    A mutable sequence of characters.  This class provides an API compatible
 *    with <code>java.lang.StringBuilder</code>, but with no guarantee of
 *    synchronization.
 *  </p>
 *
 *  @author Stephane Micheloud
 */
@SerialVersionUID(0 - 8525408645367278351L)
@throws(classOf[NullPointerException])
final class StringBuilder(initCapacity: Int, private val initValue: String)
extends (Int => Char) with Proxy {
  if (initCapacity < 0) throw new IllegalArgumentException
  if (initValue eq null) throw new NullPointerException

  /** The value is used for character storage. */
  private var value = new Array[Char](initCapacity + initValue.length)

  /** The count is the number of characters used. */
  private var count: Int = 0

  def this() = this(16, "")

  def this(capacity: Int) = this(capacity, "")

  @throws(classOf[NullPointerException])
  def this(str: String) = this(16, str)

  append(initValue)

  def self = this

  def toArray: Array[Char] = value

  def length: Int = count

  def length_=(n: Int) { setLength(n) }

  @throws(classOf[StringIndexOutOfBoundsException])
  def setLength(n: Int) {
    if (n < 0)
      throw new StringIndexOutOfBoundsException(n)
    if (n > value.length) expandCapacity(n)
    if (count < n)
      while (count < n) {
        value(count) = '\0'; count += 1
      }
    else
      count = n
  }

  def capacity: Int = value.length

  def capacity_=(n: Int) { ensureCapacity(n) }

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

  @throws(classOf[StringIndexOutOfBoundsException])
  def charAt(index: Int): Char = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    value(index)
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def apply(i: Int): Char = charAt(i)

  @throws(classOf[StringIndexOutOfBoundsException])
  def deleteCharAt(index: Int): StringBuilder = {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    compat.Platform.arraycopy(value, index + 1, value, index, count - index - 1)
    count -= 1
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def setCharAt(index: Int, c: Char) {
    if (index < 0 || index >= count)
      throw new StringIndexOutOfBoundsException(index)
    value(index) = c
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def update(i: Int, c: Char) { setCharAt(i, c) }

  @throws(classOf[StringIndexOutOfBoundsException])
  def substring(start: Int): String = substring(start, count)

  @throws(classOf[StringIndexOutOfBoundsException])
  def substring(start: Int, end: Int): String = {
    if (start < 0)
      throw new StringIndexOutOfBoundsException(start)
    if (end > count)
      throw new StringIndexOutOfBoundsException(end)
    if (start > end)
      throw new StringIndexOutOfBoundsException(end - start)
    new String(value, start, end - start)
  }

  def append(x: Any): StringBuilder =
    append(String.valueOf(x))

  /** Appends the specified string to this character sequence.
   *
   *  @param s
   *  @return
   */
  def append(s: String): StringBuilder = {
    val str = if (s == null) "null" else s
    val len = str.length
    if (len > 0) {
      val newCount = count + len
      if (newCount > value.length) expandCapacity(newCount)
      compat.Platform.arraycopy(str.toArray: Array[Char], 0, value, count, len)
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

  def append(x: Array[Char]): StringBuilder =
    append(x, 0, x.length)

  def append(x: Array[Char], offset: Int, len: Int): StringBuilder = {
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(x, offset, value, count, len)
    count = newCount
    this
  }

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

  def append(x: Int): StringBuilder =
    append(String.valueOf(x))

  def append(x: Long): StringBuilder =
    append(String.valueOf(x))

  def append(x: Float): StringBuilder =
    append(String.valueOf(x))

  def append(x: Double): StringBuilder =
    append(String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def delete(start: Int, end: Int): StringBuilder = {
    if (start < 0 || start > end)
      throw new StringIndexOutOfBoundsException(start)
    val end0 = if (end > count) count else end
    val len = end0 - start
    if (len > 0) {
      compat.Platform.arraycopy(value, start + len, value, start, count - end0)
      count -= len
    }
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def replace(start: Int, end: Int, str: String) {
    if (start < 0 || start > count || start > end)
      throw new StringIndexOutOfBoundsException(start)

    val end0 = if (end > count) count else end
    val len = str.length()
    val newCount = count + len - (end0 - start)
    if (newCount > value.length) expandCapacity(newCount)

    compat.Platform.arraycopy(value, end, value, start + len, count - end)
    compat.Platform.arraycopy(str.toArray, 0, value, start, len)
    count = newCount
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(index: Int, str: Array[Char], offset: Int, len: Int): StringBuilder = {
    if (index < 0 || index > count)
      throw new StringIndexOutOfBoundsException(index)
    if (offset < 0 || len < 0 || offset > str.length - len)
      throw new StringIndexOutOfBoundsException(
                "offset " + offset + ", len " + len +
                ", str.length " + str.length)
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, index, value, index + len, count - index)
    compat.Platform.arraycopy(str, offset, value, index, len)
    count = newCount
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Any): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: String): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException(at)
    val str = if (x == null) "null" else x
    val len = str.length
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + len, count - at)
    compat.Platform.arraycopy(str.toArray: Array[Char], 0, value, at, len)
    count = newCount
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Array[Char]): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException(at)
    val len = x.length
    val newCount = count + len
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + len, count - at)
    compat.Platform.arraycopy(x, 0, value, at, len)
    count = newCount
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Boolean): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Char): StringBuilder = {
    if (at < 0 || at > count)
      throw new StringIndexOutOfBoundsException(at)
    val newCount = count + 1
    if (newCount > value.length) expandCapacity(newCount)
    compat.Platform.arraycopy(value, at, value, at + 1, count - at)
    value(at) = x
    count = newCount
    this
  }

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Int): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Long): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Float): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[StringIndexOutOfBoundsException])
  def insert(at: Int, x: Double): StringBuilder =
    insert(at, String.valueOf(x))

  @throws(classOf[NullPointerException])
  def indexOf(str: String): Int = indexOf(str, 0)

  @throws(classOf[NullPointerException])
  def indexOf(str: String, fromIndex: Int): Int =
    StringBuilder.indexOf(value, 0, count, str.toArray, 0, str.length(), fromIndex)

  @throws(classOf[NullPointerException])
  def lastIndexOf(str: String): Int = lastIndexOf(str, count)

  @throws(classOf[NullPointerException])
  def lastIndexOf(str: String, fromIndex: Int): Int =
    StringBuilder.lastIndexOf(value, 0, count, str.toArray, 0, str.length(), fromIndex)

  def reverse(): StringBuilder = {
    var hasSurrogate = false
    val n = count - 1
    var j = (n-1) >> 1
    while (j >= 0) {
      val temp = value(j)
      val temp2 = value(n - j)
      if (!hasSurrogate)
        hasSurrogate =
          (temp >= StringBuilder.MIN_SURROGATE && temp <= StringBuilder.MAX_SURROGATE) ||
       	  (temp2 >= StringBuilder.MIN_SURROGATE && temp2 <= StringBuilder.MAX_SURROGATE)
      value(j) = temp2
      value(n - j) = temp
      j -= 1
    }
    if (hasSurrogate) {
      // Reverse back all valid surrogate pairs
      var i = 0
      while (i < count - 1) {
        val c2 = value(i)
	if (StringBuilder.isLowSurrogate(c2)) {
          val c1 = value(i + 1)
          if (StringBuilder.isHighSurrogate(c1)) {
            value(i) = c1; i += 1
            value(i) = c2
          }
        }
        i += 1
      }
    }
    this
  }

  override def toString(): String = new String(value, 0, count)

  @throws(classOf[java.io.IOException])
  private def writeObject(s: java.io.ObjectOutputStream) {
    s.defaultWriteObject()
    s.writeInt(count)
    s.writeObject(value)
  }

  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(s: java.io.ObjectInputStream ) {
    s.defaultReadObject()
    count = s.readInt()
    value = s.readObject().asInstanceOf[Array[Char]]
  }

}


object StringBuilder {

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
    val (start, end) =
      if (src.length < newLength) (src.length, newLength)
      else (newLength, src.length)
    compat.Platform.arraycopy(src, 0, dest, 0, start)
    // For any indices that are valid in the copy but not the original,
    // the copy will contain '\\u000'.
    for (i <- start until end) dest(i) = '\0'
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
