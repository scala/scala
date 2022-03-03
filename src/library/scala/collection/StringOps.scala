/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import java.lang.{StringBuilder => JStringBuilder}

import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl.{CharStringStepper, CodePointStringStepper}
import scala.collection.immutable.{ArraySeq, WrappedString}
import scala.collection.mutable.StringBuilder
import scala.math.{ScalaNumber, max, min}
import scala.reflect.ClassTag
import scala.util.matching.Regex

object StringOps {
  // just statics for companion class.
  private final val LF = 0x0A
  private final val FF = 0x0C
  private final val CR = 0x0D
  private final val SU = 0x1A

  private class StringIterator(private[this] val s: String) extends AbstractIterator[Char] {
    private[this] var pos = 0
    def hasNext: Boolean = pos < s.length
    def next(): Char = try {
      val r = s.charAt(pos)
      pos += 1
      r
    } catch { case _: IndexOutOfBoundsException => Iterator.empty.next() }
  }

  private class ReverseIterator(private[this] val s: String) extends AbstractIterator[Char] {
    private[this] var pos = s.length-1
    def hasNext: Boolean = pos >= 0
    def next(): Char = try {
      val r = s.charAt(pos)
      pos -= 1
      r
    } catch { case _: IndexOutOfBoundsException => Iterator.empty.next() }
  }

  private class GroupedIterator(s: String, groupSize: Int) extends AbstractIterator[String] {
    private[this] var pos = 0
    def hasNext: Boolean = pos < s.length
    def next(): String = {
      if(pos >= s.length) Iterator.empty.next()
      val r = s.slice(pos, pos+groupSize)
      pos += groupSize
      r
    }
  }

  /** A lazy filtered string. No filtering is applied until one of `foreach`, `map` or `flatMap` is called. */
  class WithFilter(p: Char => Boolean, s: String) {

    /** Apply `f` to each element for its side effects.
      * Note: [U] parameter needed to help scalac's type inference.
      */
    def foreach[U](f: Char => U): Unit = {
      val len = s.length
      var i = 0
      while(i < len) {
        val x = s.charAt(i)
        if(p(x)) f(x)
        i += 1
      }
    }

    /** Builds a new collection by applying a function to all chars of this filtered string.
      *
      *  @param f      the function to apply to each char.
      *  @return       a new collection resulting from applying the given function
      *                `f` to each char of this string and collecting the results.
      */
    def map[B](f: Char => B): immutable.IndexedSeq[B] = {
      val len = s.length
      val b = immutable.IndexedSeq.newBuilder[B]
      b.sizeHint(len)
      var i = 0
      while (i < len) {
        val x = s.charAt(i)
        if(p(x)) b.addOne(f(x))
        i += 1
      }
      b.result()
    }

    /** Builds a new string by applying a function to all chars of this filtered string.
      *
      *  @param f      the function to apply to each char.
      *  @return       a new string resulting from applying the given function
      *                `f` to each char of this string and collecting the results.
      */
    def map(f: Char => Char): String = {
      val len = s.length
      val sb = new JStringBuilder(len)
      var i = 0
      while (i < len) {
        val x = s.charAt(i)
        if(p(x)) sb.append(f(x))
        i += 1
      }
      sb.toString
    }

    /** Builds a new collection by applying a function to all chars of this filtered string
      * and using the elements of the resulting collections.
      *
      *  @param f      the function to apply to each char.
      *  @return       a new collection resulting from applying the given collection-valued function
      *                `f` to each char of this string and concatenating the results.
      */
    def flatMap[B](f: Char => IterableOnce[B]): immutable.IndexedSeq[B] = {
      val len = s.length
      val b = immutable.IndexedSeq.newBuilder[B]
      var i = 0
      while (i < len) {
        val x = s.charAt(i)
        if(p(x)) b.addAll(f(x))
        i += 1
      }
      b.result()
    }

    /** Builds a new string by applying a function to all chars of this filtered string
      * and using the elements of the resulting Strings.
      *
      *  @param f      the function to apply to each char.
      *  @return       a new string resulting from applying the given string-valued function
      *                `f` to each char of this string and concatenating the results.
      */
    def flatMap(f: Char => String): String = {
      val len = s.length
      val sb = new JStringBuilder
      var i = 0
      while (i < len) {
        val x = s.charAt(i)
        if(p(x)) sb.append(f(x))
        i += 1
      }
      sb.toString
    }

    /** Creates a new non-strict filter which combines this filter with the given predicate. */
    def withFilter(q: Char => Boolean): WithFilter = new WithFilter(a => p(a) && q(a), s)
  }
}

/** Provides extension methods for strings.
  *
  * Some of these methods treat strings as a plain collection of [[Char]]s
  * without any regard for Unicode handling. Unless the user takes Unicode
  * handling in to account or makes sure the strings don't require such handling,
  * these methods may result in unpaired or invalidly paired surrogate code
  * units.
  *
  * @define unicodeunaware This method treats a string as a plain sequence of
  *                        Char code units and makes no attempt to keep
  *                        surrogate pairs or codepoint sequences together.
  *                        The user is responsible for making sure such cases
  *                        are handled correctly. Failing to do so may result in
  *                        an invalid Unicode string.
  */
final class StringOps(private val s: String) extends AnyVal {
  import StringOps._

  @`inline` def view: StringView = new StringView(s)

  @`inline` def size: Int = s.length

  @`inline` def knownSize: Int = s.length

  /** Get the char at the specified index. */
  @`inline` def apply(i: Int): Char = s.charAt(i)

  def sizeCompare(otherSize: Int): Int = Integer.compare(s.length, otherSize)

  def lengthCompare(len: Int): Int = Integer.compare(s.length, len)

  def sizeIs: Int = s.length

  def lengthIs: Int = s.length

  /** Builds a new collection by applying a function to all chars of this string.
    *
    *  @param f      the function to apply to each char.
    *  @return       a new collection resulting from applying the given function
    *                `f` to each char of this string and collecting the results.
    */
  def map[B](f: Char => B): immutable.IndexedSeq[B] = {
    val len = s.length
    val dst = new Array[AnyRef](len)
    var i = 0
    while (i < len) {
      dst(i) = f(s charAt i).asInstanceOf[AnyRef]
      i += 1
    }
    new ArraySeq.ofRef(dst).asInstanceOf[immutable.IndexedSeq[B]]
  }

  /** Builds a new string by applying a function to all chars of this string.
    *
    *  @param f      the function to apply to each char.
    *  @return       a new string resulting from applying the given function
    *                `f` to each char of this string and collecting the results.
    */
  def map(f: Char => Char): String = {
    val len = s.length
    val dst = new Array[Char](len)
    var i = 0
    while (i < len) {
      dst(i) = f(s charAt i)
      i += 1
    }
    new String(dst)
  }

  /** Builds a new collection by applying a function to all chars of this string
    * and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each char.
    *  @return       a new collection resulting from applying the given collection-valued function
    *                `f` to each char of this string and concatenating the results.
    */
  def flatMap[B](f: Char => IterableOnce[B]): immutable.IndexedSeq[B] = {
    val len = s.length
    val b = immutable.IndexedSeq.newBuilder[B]
    var i = 0
    while (i < len) {
      b.addAll(f(s.charAt(i)))
      i += 1
    }
    b.result()
  }

  /** Builds a new string by applying a function to all chars of this string
    * and using the elements of the resulting strings.
    *
    *  @param f      the function to apply to each char.
    *  @return       a new string resulting from applying the given string-valued function
    *                `f` to each char of this string and concatenating the results.
    */
  def flatMap(f: Char => String): String = {
    val len = s.length
    val sb = new JStringBuilder
    var i = 0
    while (i < len) {
      sb append f(s.charAt(i))
      i += 1
    }
    sb.toString
  }

  /** Builds a new String by applying a partial function to all chars of this String
    * on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the String.
    *  @return       a new String resulting from applying the given partial function
    *                `pf` to each char on which it is defined and collecting the results.
    */
  def collect(pf: PartialFunction[Char, Char]): String = {
    var i = 0
    var matched = true
    def d(x: Char): Char = {
      matched = false
      0
    }
    val b = new StringBuilder
    while(i < s.length) {
      matched = true
      val v = pf.applyOrElse(s.charAt(i), d)
      if(matched) b += v
      i += 1
    }
    b.result()
  }

  /** Builds a new collection by applying a partial function to all chars of this String
    * on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the String.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection resulting from applying the given partial function
    *                `pf` to each char on which it is defined and collecting the results.
    */
  def collect[B](pf: PartialFunction[Char, B]): immutable.IndexedSeq[B] = {
    var i = 0
    var matched = true
    def d(x: Char): B = {
      matched = false
      null.asInstanceOf[B]
    }
    val b = immutable.IndexedSeq.newBuilder[B]
    while(i < s.length) {
      matched = true
      val v = pf.applyOrElse(s.charAt(i), d)
      if(matched) b += v
      i += 1
    }
    b.result()
  }

  /** Returns a new collection containing the chars from this string followed by the elements from the
    * right hand operand.
    *
    *  @param suffix the collection to append.
    *  @return       a new collection which contains all chars
    *                of this string followed by all elements of `suffix`.
    */
  def concat[B >: Char](suffix: IterableOnce[B]): immutable.IndexedSeq[B] = {
    val b = immutable.IndexedSeq.newBuilder[B]
    val k = suffix.knownSize
    b.sizeHint(s.length + (if(k >= 0) k else 16))
    b.addAll(new WrappedString(s))
    b.addAll(suffix)
    b.result()
  }

  /** Returns a new string containing the chars from this string followed by the chars from the
    * right hand operand.
    *
    *  @param suffix the collection to append.
    *  @return       a new string which contains all chars
    *                of this string followed by all chars of `suffix`.
    */
  def concat(suffix: IterableOnce[Char]): String = {
    val k = suffix.knownSize
    val sb = new JStringBuilder(s.length + (if(k >= 0) k else 16))
    sb.append(s)
    for (ch <- suffix.iterator) sb.append(ch)
    sb.toString
  }

  /** Returns a new string containing the chars from this string followed by the chars from the
    * right hand operand.
    *
    *  @param suffix the string to append.
    *  @return       a new string which contains all chars
    *                of this string followed by all chars of `suffix`.
    */
  @`inline` def concat(suffix: String): String = s + suffix

  /** Alias for `concat` */
  @`inline` def ++[B >: Char](suffix: Iterable[B]): immutable.IndexedSeq[B] = concat(suffix)

  /** Alias for `concat` */
  @`inline` def ++(suffix: IterableOnce[Char]): String = concat(suffix)

  /** Alias for `concat` */
  def ++(xs: String): String = concat(xs)

  /** Returns a collection with an element appended until a given target length is reached.
    *
    *  @param  len   the target length
    *  @param  elem  the padding value
    *  @return a collection consisting of
    *          this string followed by the minimal number of occurrences of `elem` so
    *          that the resulting collection has a length of at least `len`.
    */
  def padTo[B >: Char](len: Int, elem: B): immutable.IndexedSeq[B]  = {
    val sLen = s.length
    if (sLen >= len) new WrappedString(s) else {
      val b = immutable.IndexedSeq.newBuilder[B]
      b.sizeHint(len)
      b.addAll(new WrappedString(s))
      var i = sLen
      while (i < len) {
        b.addOne(elem)
        i += 1
      }
      b.result()
    }
  }

  /** Returns a string with a char appended until a given target length is reached.
    *
    *  @param   len   the target length
    *  @param   elem  the padding value
    *  @return a string consisting of
    *          this string followed by the minimal number of occurrences of `elem` so
    *          that the resulting string has a length of at least `len`.
    */
  def padTo(len: Int, elem: Char): String = {
    val sLen = s.length
    if (sLen >= len) s else {
      val sb = new JStringBuilder(len)
      sb.append(s)
      // With JDK 11, this can written as:
      // sb.append(String.valueOf(elem).repeat(len - sLen))
      var i = sLen
      while (i < len) {
        sb.append(elem)
        i += 1
      }
      sb.toString
    }
  }

  /** A copy of the string with an element prepended */
  def prepended[B >: Char](elem: B): immutable.IndexedSeq[B] = {
    val b = immutable.IndexedSeq.newBuilder[B]
    b.sizeHint(s.length + 1)
    b.addOne(elem)
    b.addAll(new WrappedString(s))
    b.result()
  }

  /** Alias for `prepended` */
  @`inline` def +: [B >: Char] (elem: B): immutable.IndexedSeq[B] = prepended(elem)

  /** A copy of the string with an char prepended */
  def prepended(c: Char): String =
    new JStringBuilder(s.length + 1).append(c).append(s).toString

  /** Alias for `prepended` */
  @`inline` def +: (c: Char): String = prepended(c)

  /** A copy of the string with all elements from a collection prepended */
  def prependedAll[B >: Char](prefix: IterableOnce[B]): immutable.IndexedSeq[B] = {
    val b = immutable.IndexedSeq.newBuilder[B]
    val k = prefix.knownSize
    b.sizeHint(s.length + (if(k >= 0) k else 16))
    b.addAll(prefix)
    b.addAll(new WrappedString(s))
    b.result()
  }

  /** Alias for `prependedAll` */
  @`inline` def ++: [B >: Char] (prefix: IterableOnce[B]): immutable.IndexedSeq[B] = prependedAll(prefix)

  /** A copy of the string with another string prepended */
  def prependedAll(prefix: String): String = prefix + s

  /** Alias for `prependedAll` */
  @`inline` def ++: (prefix: String): String = prependedAll(prefix)

  /** A copy of the string with an element appended */
  def appended[B >: Char](elem: B): immutable.IndexedSeq[B] = {
    val b = immutable.IndexedSeq.newBuilder[B]
    b.sizeHint(s.length + 1)
    b.addAll(new WrappedString(s))
    b.addOne(elem)
    b.result()
  }

  /** Alias for `appended` */
  @`inline` def :+ [B >: Char](elem: B): immutable.IndexedSeq[B] = appended(elem)

  /** A copy of the string with an element appended */
  def appended(c: Char): String =
    new JStringBuilder(s.length + 1).append(s).append(c).toString

  /** Alias for `appended` */
  @`inline` def :+ (c: Char): String = appended(c)

  /** A copy of the string with all elements from a collection appended */
  @`inline` def appendedAll[B >: Char](suffix: IterableOnce[B]): immutable.IndexedSeq[B] =
    concat(suffix)

  /** Alias for `appendedAll` */
  @`inline` def :++ [B >: Char](suffix: IterableOnce[B]): immutable.IndexedSeq[B] =
    concat(suffix)

  /** A copy of the string with another string appended */
  @`inline` def appendedAll(suffix: String): String = s + suffix

  /** Alias for `appendedAll` */
  @`inline` def :++ (suffix: String): String = s + suffix

  /** Produces a new collection where a slice of characters in this string is replaced by another collection.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original string appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced char
    *  @param  other    the replacement collection
    *  @param  replaced the number of chars to drop in the original string
    *  @return          a new collection consisting of all chars of this string
    *                   except that `replaced` chars starting from `from` are replaced
    *                   by `other`.
    */
  def patch[B >: Char](from: Int, other: IterableOnce[B], replaced: Int): immutable.IndexedSeq[B] = {
    val len = s.length
    @`inline` def slc(off: Int, length: Int): WrappedString =
      new WrappedString(s.substring(off, off+length))
    val b = immutable.IndexedSeq.newBuilder[B]
    val k = other.knownSize
    if(k >= 0) b.sizeHint(len + k - replaced)
    val chunk1 = if(from > 0) min(from, len) else 0
    if(chunk1 > 0) b.addAll(slc(0, chunk1))
    b ++= other
    val remaining = len - chunk1 - replaced
    if(remaining > 0) b.addAll(slc(len - remaining, remaining))
    b.result()
  }

  /** Produces a new collection where a slice of characters in this string is replaced by another collection.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original string appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced char
    *  @param  other    the replacement string
    *  @param  replaced the number of chars to drop in the original string
    *  @return          a new string consisting of all chars of this string
    *                   except that `replaced` chars starting from `from` are replaced
    *                   by `other`.
    *  @note            $unicodeunaware
    */
  def patch(from: Int, other: IterableOnce[Char], replaced: Int): String =
    patch(from, other.iterator.mkString, replaced)

  /** Produces a new string where a slice of characters in this string is replaced by another string.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original string appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced char
    *  @param  other    the replacement string
    *  @param  replaced the number of chars to drop in the original string
    *  @return          a new string consisting of all chars of this string
    *                   except that `replaced` chars starting from `from` are replaced
    *                   by `other`.
    *  @note            $unicodeunaware
    */
  def patch(from: Int, other: String, replaced: Int): String = {
    val len = s.length
    val sb = new JStringBuilder(len + other.size - replaced)
    val chunk1 = if(from > 0) min(from, len) else 0
    if(chunk1 > 0) sb.append(s, 0, chunk1)
    sb.append(other)
    val remaining = len - chunk1 - replaced
    if(remaining > 0) sb.append(s, len - remaining, len)
    sb.toString
  }

  /** A copy of this string with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new string which is a copy of this string with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *  @note   $unicodeunaware
    */
  def updated(index: Int, elem: Char): String = {
    val sb = new JStringBuilder(s.length).append(s)
    sb.setCharAt(index, elem)
    sb.toString
  }

  /** Tests whether this string contains the given character.
    *
    *  @param elem  the character to test.
    *  @return     `true` if this string has an element that is equal (as
    *              determined by `==`) to `elem`, `false` otherwise.
    */
  def contains(elem: Char): Boolean = s.indexOf(elem) >= 0

  /** Displays all elements of this string in a string using start, end, and
    * separator strings.
    *
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      The resulting string
    *               begins with the string `start` and ends with the string
    *               `end`. Inside, the string chars of this string are separated by
    *               the string `sep`.
    *  @note        $unicodeunaware
    */
  final def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Displays all elements of this string in a string using a separator string.
    *
    *  @param sep   the separator string.
    *  @return      In the resulting string
    *               the chars of this string are separated by the string `sep`.
    *  @note        $unicodeunaware
    */
  @inline final def mkString(sep: String): String =
    if (sep.isEmpty || s.length < 2) s
    else mkString("", sep, "")

  /** Returns this string */
  @inline final def mkString: String = s

  /** Appends this string to a string builder. */
  @inline final def addString(b: StringBuilder): StringBuilder = b.append(s)

  /** Appends this string to a string builder using a separator string. */
  @inline final def addString(b: StringBuilder, sep: String): StringBuilder =
    addString(b, "", sep, "")

  /** Appends this string to a string builder using start, end and separator strings. */
  final def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    val jsb = b.underlying
    if (start.length != 0) jsb.append(start)
    val len = s.length
    if (len != 0) {
      if (sep.isEmpty) jsb.append(s)
      else {
        jsb.ensureCapacity(jsb.length + len + end.length + (len - 1) * sep.length)
        jsb.append(s.charAt(0))
        var i = 1
        while (i < len) {
          jsb.append(sep)
          jsb.append(s.charAt(i))
          i += 1
        }
      }
    }
    if (end.length != 0) jsb.append(end)
    b
  }

  /** Selects an interval of elements.  The returned string is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *
    *  @param from   the lowest index to include from this string.
    *  @param until  the lowest index to EXCLUDE from this string.
    *  @return  a string containing the elements greater than or equal to
    *           index `from` extending up to (but not including) index `until`
    *           of this string.
    *  @note    $unicodeunaware
    */
  def slice(from: Int, until: Int): String = {
    val start = from max 0
    val end   = until min s.length

    if (start >= end) ""
    else s.substring(start, end)
  }

  // Note: String.repeat is added in JDK 11.
  /** Return the current string concatenated `n` times.
    */
  def *(n: Int): String = {
    if (n <= 0) {
      ""
    } else {
      val sb = new JStringBuilder(s.length * n)
      var i = 0
      while (i < n) {
        sb.append(s)
        i += 1
      }
      sb.toString
    }
  }

  @`inline` private[this] def isLineBreak(c: Char) = c == CR || c == LF
  @`inline` private[this] def isLineBreak2(c0: Char, c: Char) = c0 == CR && c == LF

  /** Strip the trailing line separator from this string if there is one.
   *  The line separator is taken as `"\n"`, `"\r"`, or `"\r\n"`.
   */
  def stripLineEnd: String =
    if (s.isEmpty) s
    else {
      var i = s.length - 1
      val last = apply(i)
      if (!isLineBreak(last)) s
      else {
        if (i > 0 && isLineBreak2(apply(i - 1), last)) i -= 1
        s.substring(0, i)
      }
    }

  /** Return an iterator of all lines embedded in this string,
   *  including trailing line separator characters.
   *
   *  The empty string yields an empty iterator.
   */
  def linesWithSeparators: Iterator[String] = linesSeparated(stripped = false)

  /** Lines in this string, where a line is terminated by
   *  `"\n"`, `"\r"`, `"\r\n"`, or the end of the string.
   *  A line may be empty. Line terminators are removed.
   */
  def linesIterator: Iterator[String] = linesSeparated(stripped = true)

  // if `stripped`, exclude the line separators
  private def linesSeparated(stripped: Boolean): Iterator[String] = new AbstractIterator[String] {
    def hasNext: Boolean = !done
    def next(): String = if (done) Iterator.empty.next() else advance()

    private[this] val len = s.length
    private[this] var index = 0
    @`inline` private def done = index >= len
    private def advance(): String = {
      val start = index
      while (!done && !isLineBreak(apply(index))) index += 1
      var end   = index
      if (!done) {
        val c = apply(index)
        index += 1
        if (!done && isLineBreak2(c, apply(index))) index += 1
        if (!stripped) end = index
      }
      s.substring(start, end)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
    *  end characters; i.e., apply `.stripLineEnd` to all lines
    *  returned by `linesWithSeparators`.
    */
  @deprecated("Use `linesIterator`, because JDK 11 adds a `lines` method on String", "2.13.0")
  def lines: Iterator[String] = linesIterator

  /** Returns this string with first character converted to upper case.
    * If the first character of the string is capitalized, it is returned unchanged.
    * This method does not convert characters outside the Basic Multilingual Plane (BMP).
    */
  def capitalize: String =
    if (s == null || s.length == 0 || !s.charAt(0).isLower) s
    else updated(0, s.charAt(0).toUpper)

  /** Returns this string with the given `prefix` stripped. If this string does not
    *  start with `prefix`, it is returned unchanged.
    */
  def stripPrefix(prefix: String) =
    if (s startsWith prefix) s.substring(prefix.length)
    else s

  /** Returns this string with the given `suffix` stripped. If this string does not
    *  end with `suffix`, it is returned unchanged.
    */
  def stripSuffix(suffix: String) =
    if (s endsWith suffix) s.substring(0, s.length - suffix.length)
    else s

  /** Replace all literal occurrences of `literal` with the literal string `replacement`.
    * This method is equivalent to [[java.lang.String#replace(CharSequence,CharSequence)]].
    *
    * @param    literal     the string which should be replaced everywhere it occurs
    * @param    replacement the replacement string
    * @return               the resulting string
    */
  @deprecated("Use `s.replace` as an exact replacement", "2.13.2")
  def replaceAllLiterally(literal: String, replacement: String): String = s.replace(literal, replacement)

  /** For every line in this string:
    *
    *  Strip a leading prefix consisting of blanks or control characters
    *  followed by `marginChar` from the line.
    */
  def stripMargin(marginChar: Char): String = {
    val sb = new JStringBuilder(s.length)
    for (line <- linesWithSeparators) {
      val len = line.length
      var index = 0
      while (index < len && line.charAt(index) <= ' ') index += 1
      val stripped =
        if (index < len && line.charAt(index) == marginChar) line.substring(index + 1)
        else line
      sb.append(stripped)
    }
    sb.toString
  }

  /** For every line in this string:
    *
    *  Strip a leading prefix consisting of blanks or control characters
    *  followed by `|` from the line.
    */
  def stripMargin: String = stripMargin('|')

  private[this] def escape(ch: Char): String = if (
    (ch >= 'a') && (ch <= 'z') ||
      (ch >= 'A') && (ch <= 'Z') ||
      (ch >= '0' && ch <= '9')) ch.toString
  else "\\" + ch

  /** Split this string around the separator character
    *
    * If this string is the empty string, returns an array of strings
    * that contains a single empty string.
    *
    * If this string is not the empty string, returns an array containing
    * the substrings terminated by the start of the string, the end of the
    * string or the separator character, excluding empty trailing substrings
    *
    * If the separator character is a surrogate character, only split on
    * matching surrogate characters if they are not part of a surrogate pair
    *
    * The behaviour follows, and is implemented in terms of <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#split-java.lang.String-">String.split(re: String)</a>
    *
    *
    * @example {{{
    * "a.b".split('.') //returns Array("a", "b")
    *
    * //splitting the empty string always returns the array with a single
    * //empty string
    * "".split('.') //returns Array("")
    *
    * //only trailing empty substrings are removed
    * "a.".split('.') //returns Array("a")
    * ".a.".split('.') //returns Array("", "a")
    * "..a..".split('.') //returns Array("", "", "a")
    *
    * //all parts are empty and trailing
    * ".".split('.') //returns Array()
    * "..".split('.') //returns Array()
    *
    * //surrogate pairs
    * val high = 0xD852.toChar
    * val low = 0xDF62.toChar
    * val highstring = high.toString
    * val lowstring = low.toString
    *
    * //well-formed surrogate pairs are not split
    * val highlow = highstring + lowstring
    * highlow.split(high) //returns Array(highlow)
    *
    * //bare surrogate characters are split
    * val bare = "_" + highstring + "_"
    * bare.split(high) //returns Array("_", "_")
    *
    *  }}}
    *
    * @param separator the character used as a delimiter
    */
  def split(separator: Char): Array[String] = s.split(escape(separator))

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+escape(_)) + "]"
    s.split(re)
  }

  /** You can follow a string with `.r`, turning it into a `Regex`. E.g.
    *
    *  `"""A\w*""".r`   is the regular expression for ASCII-only identifiers starting with `A`.
    *
    *  `"""(?<month>\d\d)-(?<day>\d\d)-(?<year>\d\d\d\d)""".r` matches dates
    *  and provides its subcomponents through groups named "month", "day" and
    *  "year".
    */
  def r: Regex = new Regex(s)

  /** You can follow a string with `.r(g1, ... , gn)`, turning it into a `Regex`,
    *  with group names g1 through gn.
    *
    *  `"""(\d\d)-(\d\d)-(\d\d\d\d)""".r("month", "day", "year")` matches dates
    *  and provides its subcomponents through groups named "month", "day" and
    *  "year".
    *
    *  @param groupNames The names of the groups in the pattern, in the order they appear.
    */
  @deprecated("use inline group names like (?<year>X) instead", "2.13.7")
  def r(groupNames: String*): Regex = new Regex(s, groupNames: _*)

  /**
   * @throws java.lang.IllegalArgumentException  If the string does not contain a parsable `Boolean`.
   */
  def toBoolean: Boolean               = toBooleanImpl(s)

  /**
   * Try to parse as a `Boolean`
   * @return `Some(true)` if the string is "true" case insensitive,
   * `Some(false)` if the string is "false" case insensitive,
   * and `None` if the string is anything else
   * @throws java.lang.NullPointerException if the string is `null`
   */
  def toBooleanOption: Option[Boolean] = StringParsers.parseBool(s)

  /**
    * Parse as a `Byte` (string must contain only decimal digits and optional leading `-` or `+`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Byte`.
    */
  def toByte: Byte                     = java.lang.Byte.parseByte(s)

  /**
   * Try to parse as a `Byte`
   * @return `Some(value)` if the string contains a valid byte value, otherwise `None`
   * @throws java.lang.NullPointerException if the string is `null`
   */
  def toByteOption: Option[Byte]       = StringParsers.parseByte(s)

  /**
    * Parse as a `Short` (string must contain only decimal digits and optional leading `-` or `+`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Short`.
    */
  def toShort: Short                   = java.lang.Short.parseShort(s)

  /**
   * Try to parse as a `Short`
   * @return `Some(value)` if the string contains a valid short value, otherwise `None`
   * @throws java.lang.NullPointerException if the string is `null`
   */
  def toShortOption: Option[Short]     = StringParsers.parseShort(s)

  /**
    * Parse as an `Int` (string must contain only decimal digits and optional leading `-` or `+`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Int`.
    */
  def toInt: Int                       = java.lang.Integer.parseInt(s)

  /**
   * Try to parse as an `Int`
   * @return `Some(value)` if the string contains a valid Int value, otherwise `None`
   * @throws java.lang.NullPointerException if the string is `null`
   */
  def toIntOption: Option[Int]         = StringParsers.parseInt(s)

  /**
    * Parse as a `Long` (string must contain only decimal digits and optional leading `-` or `+`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Long`.
    */
  def toLong: Long                     = java.lang.Long.parseLong(s)

  /**
   * Try to parse as a `Long`
   * @return `Some(value)` if the string contains a valid long value, otherwise `None`
   * @throws java.lang.NullPointerException if the string is `null`
   */
  def toLongOption: Option[Long]       = StringParsers.parseLong(s)

  /**
    * Parse as a `Float` (surrounding whitespace is removed with a `trim`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Float`.
    * @throws java.lang.NullPointerException  If the string is null.
    */
  def toFloat: Float                   = java.lang.Float.parseFloat(s)

  /**
   * Try to parse as a `Float`
   * @return `Some(value)` if the string is a parsable `Float`, `None` otherwise
   * @throws java.lang.NullPointerException If the string is null
   */
  def toFloatOption: Option[Float]     = StringParsers.parseFloat(s)

  /**
    * Parse as a `Double` (surrounding whitespace is removed with a `trim`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Double`.
    * @throws java.lang.NullPointerException  If the string is null.
    */
  def toDouble: Double                 = java.lang.Double.parseDouble(s)

  /**
   * Try to parse as a `Double`
   * @return `Some(value)` if the string is a parsable `Double`, `None` otherwise
   * @throws java.lang.NullPointerException If the string is null
   */
  def toDoubleOption: Option[Double]   = StringParsers.parseDouble(s)

  private[this] def toBooleanImpl(s: String): Boolean =
    if (s == null) throw new IllegalArgumentException("For input string: \"null\"")
    else if (s.equalsIgnoreCase("true")) true
    else if (s.equalsIgnoreCase("false")) false
    else throw new IllegalArgumentException("For input string: \""+s+"\"")

  def toArray[B >: Char](implicit tag: ClassTag[B]): Array[B] =
    if (tag == ClassTag.Char) s.toCharArray.asInstanceOf[Array[B]]
    else new WrappedString(s).toArray[B]

  private[this] def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.asInstanceOf[AnyRef]
  }

  /** Uses the underlying string as a pattern (in a fashion similar to
    *  printf in C), and uses the supplied arguments to fill in the
    *  holes.
    *
    *    The interpretation of the formatting patterns is described in
    *    [[java.util.Formatter]], with the addition that
    *    classes deriving from `ScalaNumber` (such as [[scala.BigInt]] and
    *    [[scala.BigDecimal]]) are unwrapped to pass a type which `Formatter`
    *    understands.
    *
    *  @param args the arguments used to instantiating the pattern.
    *  @throws java.lang.IllegalArgumentException
    */
  def format(args : Any*): String =
    java.lang.String.format(s, args map unwrapArg: _*)

  /** Like `format(args*)` but takes an initial `Locale` parameter
    *  which influences formatting as in `java.lang.String`'s format.
    *
    *    The interpretation of the formatting patterns is described in
    *    [[java.util.Formatter]], with the addition that
    *    classes deriving from `ScalaNumber` (such as `scala.BigInt` and
    *    `scala.BigDecimal`) are unwrapped to pass a type which `Formatter`
    *    understands.
    *
    *  @param l    an instance of `java.util.Locale`
    *  @param args the arguments used to instantiating the pattern.
    *  @throws java.lang.IllegalArgumentException
    */
  def formatLocal(l: java.util.Locale, args: Any*): String =
    java.lang.String.format(l, s, args map unwrapArg: _*)

  def compare(that: String): Int = s.compareTo(that)

  /** Returns true if `this` is less than `that` */
  def < (that: String): Boolean = compare(that) <  0

  /** Returns true if `this` is greater than `that`. */
  def > (that: String): Boolean = compare(that) >  0

  /** Returns true if `this` is less than or equal to `that`. */
  def <= (that: String): Boolean = compare(that) <= 0

  /** Returns true if `this` is greater than or equal to `that`. */
  def >= (that: String): Boolean = compare(that) >= 0

  /** Counts the number of chars in this string which satisfy a predicate */
  def count(p: (Char) => Boolean): Int = {
    var i, res = 0
    val len = s.length
    while(i < len) {
      if(p(s.charAt(i))) res += 1
      i += 1
    }
    res
  }

  /** Apply `f` to each element for its side effects.
    * Note: [U] parameter needed to help scalac's type inference.
    */
  def foreach[U](f: Char => U): Unit = {
    val len = s.length
    var i = 0
    while(i < len) {
      f(s.charAt(i))
      i += 1
    }
  }

  /** Tests whether a predicate holds for all chars of this string.
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if this string is empty or the given predicate `p`
    *                 holds for all chars of this string, otherwise `false`.
    */
  def forall(@deprecatedName("f", "2.13.3") p: Char => Boolean): Boolean = {
    var i = 0
    val len = s.length
    while(i < len) {
      if(!p(s.charAt(i))) return false
      i += 1
    }
    true
  }

  /** Applies a binary operator to a start value and all chars of this string,
    * going left to right.
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive chars of this string,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the chars of this string.
    *           Returns `z` if this string is empty.
    */
  def foldLeft[B](z: B)(op: (B, Char) => B): B = {
    var v = z
    var i = 0
    val len = s.length
    while(i < len) {
      v = op(v, s.charAt(i))
      i += 1
    }
    v
  }

  /** Applies a binary operator to all chars of this string and a start value,
    * going right to left.
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive chars of this string,
    *           going right to left with the start value `z` on the right:
    *           {{{
    *             op(x_1, op(x_2, ... op(x_n, z)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the chars of this string.
    *           Returns `z` if this string is empty.
    */
  def foldRight[B](z: B)(op: (Char, B) => B): B = {
    var v = z
    var i = s.length - 1
    while(i >= 0) {
      v = op(s.charAt(i), v)
      i -= 1
    }
    v
  }

  /** Folds the chars of this string using the specified associative binary operator.
    *
    *  @tparam A1     a type parameter for the binary operator, a supertype of Char.
    *  @param z       a neutral element for the fold operation; may be added to the result
    *                 an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
    *                 0 for addition, or 1 for multiplication).
    *  @param op      a binary operator that must be associative.
    *  @return        the result of applying the fold operator `op` between all the chars and `z`, or `z` if this string is empty.
    */
  @`inline` def fold[A1 >: Char](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  /** Selects the first char of this string.
    *  @return  the first char of this string.
    *  @throws NoSuchElementException if the string is empty.
    */
  def head: Char = if(s.isEmpty) throw new NoSuchElementException("head of empty String") else s.charAt(0)

  /** Optionally selects the first char.
    *  @return  the first char of this string if it is nonempty,
    *           `None` if it is empty.
    */
  def headOption: Option[Char] =
    if(s.isEmpty) None else Some(s.charAt(0))

  /** Selects the last char of this string.
    *  @return  the last char of this string.
    *  @throws NoSuchElementException if the string is empty.
    */
  def last: Char = if(s.isEmpty) throw new NoSuchElementException("last of empty String") else s.charAt(s.length-1)

  /** Optionally selects the last char.
    *  @return  the last char of this string if it is nonempty,
    *           `None` if it is empty.
    */
  def lastOption: Option[Char] =
    if(s.isEmpty) None else Some(s.charAt(s.length-1))

  /** Produces the range of all indices of this string.
    *
    *  @return  a `Range` value from `0` to one less than the length of this string.
    */
  def indices: Range = Range(0, s.length)

  /** Iterator can be used only once */
  def iterator: Iterator[Char] = new StringIterator(s)

  /** Stepper can be used with Java 8 Streams. This method is equivalent to a call to
    * [[charStepper]]. See also [[codePointStepper]].
    */
  @`inline` def stepper: IntStepper with EfficientSplit = charStepper

  /** Steps over characters in this string. Values are packed in `Int` for efficiency
    * and compatibility with Java 8 Streams which have an efficient specialization for `Int`.
    */
  @`inline` def charStepper: IntStepper with EfficientSplit = new CharStringStepper(s, 0, s.length)

  /** Steps over code points in this string.
    */
  @`inline` def codePointStepper: IntStepper with EfficientSplit = new CodePointStringStepper(s, 0, s.length)

  /** Tests whether the string is not empty. */
  @`inline` def nonEmpty: Boolean = !s.isEmpty

  /** Returns new sequence with elements in reversed order.
    * @note $unicodeunaware
    */
  def reverse: String = new JStringBuilder(s).reverse().toString

  /** An iterator yielding chars in reversed order.
    *
    * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but implemented more efficiently.
    *
    *  @return  an iterator yielding the chars of this string in reversed order
    */
  def reverseIterator: Iterator[Char] = new ReverseIterator(s)

  /** Creates a non-strict filter of this string.
    *
    *  @note the difference between `c filter p` and `c withFilter p` is that
    *        the former creates a new string, whereas the latter only
    *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
    *        and `withFilter` operations.
    *
    *  @param p   the predicate used to test elements.
    *  @return    an object of class `stringOps.WithFilter`, which supports
    *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
    *             All these operations apply to those chars of this string
    *             which satisfy the predicate `p`.
    */
  def withFilter(p: Char => Boolean): StringOps.WithFilter = new StringOps.WithFilter(p, s)

  /** The rest of the string without its first char.
    * @note $unicodeunaware
    */
  def tail: String = slice(1, s.length)

  /** The initial part of the string without its last char.
    * @note $unicodeunaware
    */
  def init: String = slice(0, s.length-1)

  /** A string containing the first `n` chars of this string.
    * @note $unicodeunaware
    */
  def take(n: Int): String = slice(0, min(n, s.length))

  /** The rest of the string without its `n` first chars.
    * @note $unicodeunaware
    */
  def drop(n: Int): String = slice(min(n, s.length), s.length)

  /** A string containing the last `n` chars of this string.
    * @note $unicodeunaware
    */
  def takeRight(n: Int): String = drop(s.length - max(n, 0))

  /** The rest of the string without its `n` last chars.
    * @note $unicodeunaware
    */
  def dropRight(n: Int): String = take(s.length - max(n, 0))

  /** Iterates over the tails of this string. The first value will be this
    * string and the final one will be an empty string, with the intervening
    * values the results of successive applications of `tail`.
    *
    *  @return   an iterator over all the tails of this string
    *  @note     $unicodeunaware
    */
  def tails: Iterator[String] = iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this string. The first value will be this
    * string and the final one will be an empty string, with the intervening
    * values the results of successive applications of `init`.
    *
    *  @return  an iterator over all the inits of this string
    *  @note    $unicodeunaware
    */
  def inits: Iterator[String] = iterateUntilEmpty(_.init)

  // A helper for tails and inits.
  private[this] def iterateUntilEmpty(f: String => String): Iterator[String] =
    Iterator.iterate(s)(f).takeWhile(x => !x.isEmpty) ++ Iterator.single("")

  /** Selects all chars of this string which satisfy a predicate. */
  def filter(pred: Char => Boolean): String = {
    val len = s.length
    val sb = new JStringBuilder(len)
    var i = 0
    while (i < len) {
      val x = s.charAt(i)
      if(pred(x)) sb.append(x)
      i += 1
    }
    if(len == sb.length()) s else sb.toString
  }

  /** Selects all chars of this string which do not satisfy a predicate. */
  @`inline` def filterNot(pred: Char => Boolean): String = filter(c => !pred(c))

  /** Copy chars of this string to an array.
    * Fills the given array `xs` starting at index 0.
    * Copying will stop once either the entire string has been copied
    * or the end of the array is reached
    *
    *  @param  xs     the array to fill.
    */
  @`inline` def copyToArray(xs: Array[Char]): Int =
    copyToArray(xs, 0, Int.MaxValue)

  /** Copy chars of this string to an array.
    * Fills the given array `xs` starting at index `start`.
    * Copying will stop once either the entire string has been copied
    * or the end of the array is reached
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index.
    */
  @`inline` def copyToArray(xs: Array[Char], start: Int): Int =
    copyToArray(xs, start, Int.MaxValue)

  /** Copy chars of this string to an array.
    * Fills the given array `xs` starting at index `start` with at most `len` chars.
    * Copying will stop once either the entire string has been copied,
    * or the end of the array is reached or `len` chars have been copied.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index.
    *  @param  len    the maximal number of elements to copy.
    */
  def copyToArray(xs: Array[Char], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(s.length, xs.length, start, len)
    if (copied > 0) {
      s.getChars(0, copied, xs, start)
    }
    copied
  }

  /** Finds index of the first char satisfying some predicate after or at some start index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this string that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def indexWhere(p: Char => Boolean, from: Int = 0): Int = {
    val len = s.length
    var i = from
    while(i < len) {
      if(p(s.charAt(i))) return i
      i += 1
    }
    -1
  }

  /** Finds index of the last char satisfying some predicate before or at some end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   end   the end index
    *  @return  the index `<= end` of the last element of this string that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def lastIndexWhere(p: Char => Boolean, end: Int = Int.MaxValue): Int = {
    val len = s.length
    var i = min(end, len-1)
    while(i >= 0) {
      if(p(s.charAt(i))) return i
      i -= 1
    }
    -1
  }

  /** Tests whether a predicate holds for at least one char of this string. */
  def exists(p: Char => Boolean): Boolean = indexWhere(p) != -1

  /** Finds the first char of the string satisfying a predicate, if any.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the string
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: Char => Boolean): Option[Char] = indexWhere(p) match {
    case -1 => None
    case i => Some(s.charAt(i))
  }

  /** Drops longest prefix of chars that satisfy a predicate.
    *
    *  @param   p  The predicate used to test elements.
    *  @return  the longest suffix of this string whose first element
    *           does not satisfy the predicate `p`.
    */
  def dropWhile(p: Char => Boolean): String = indexWhere(c => !p(c)) match {
    case -1 => ""
    case i => s.substring(i)
  }

  /** Takes longest prefix of chars that satisfy a predicate. */
  def takeWhile(p: Char => Boolean): String = indexWhere(c => !p(c)) match {
    case -1 => s
    case i => s.substring(0, i)
  }

  /** Splits this string into two at a given position.
    * Note: `c splitAt n` is equivalent to `(c take n, c drop n)`.
    *
    *  @param n the position at which to split.
    *  @return  a pair of strings consisting of the first `n`
    *           chars of this string, and the other chars.
    *  @note    $unicodeunaware
    */
  def splitAt(n: Int): (String, String) = (take(n), drop(n))

  /** Splits this string into a prefix/suffix pair according to a predicate.
    *
    *  Note: `c span p`  is equivalent to (but more efficient than)
    *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
    *  predicate `p` does not cause any side-effects.
    *
    *  @param p the test predicate
    *  @return  a pair consisting of the longest prefix of this string whose
    *           chars all satisfy `p`, and the rest of this string.
    */
  def span(p: Char => Boolean): (String, String) = indexWhere(c => !p(c)) match {
    case -1 => (s, "")
    case i => (s.substring(0, i), s.substring(i))
  }

  /** Partitions elements in fixed size strings.
    *  @see [[scala.collection.Iterator]], method `grouped`
    *
    *  @param size the number of elements per group
    *  @return An iterator producing strings of size `size`, except the
    *          last will be less than size `size` if the elements don't divide evenly.
    *  @note $unicodeunaware
    */
  def grouped(size: Int): Iterator[String] = new StringOps.GroupedIterator(s, size)

  /** A pair of, first, all chars that satisfy predicate `p` and, second, all chars that do not. */
  def partition(p: Char => Boolean): (String, String) = {
    val res1, res2 = new JStringBuilder
    var i = 0
    val len = s.length
    while(i < len) {
      val x = s.charAt(i)
      (if(p(x)) res1 else res2).append(x)
      i += 1
    }
    (res1.toString, res2.toString)
  }

  /** Applies a function `f` to each character of the string and returns a pair of strings: the first one
    *  made of those characters returned by `f` that were wrapped in [[scala.util.Left]], and the second
    *  one made of those wrapped in [[scala.util.Right]].
    *
    *  Example:
    *  {{{
    *    val xs = "1one2two3three" partitionMap { c =>
    *      if (c > 'a') Left(c) else Right(c)
    *    }
    *    // xs == ("onetwothree", "123")
    *  }}}
    *
    *  @param f    the 'split function' mapping the elements of this string to an [[scala.util.Either]]
    *
    *  @return     a pair of strings: the first one made of those characters returned by `f` that were wrapped in [[scala.util.Left]],
    *              and the second one made of those wrapped in [[scala.util.Right]].
    */
  def partitionMap(f: Char => Either[Char,Char]): (String, String) = {
    val res1, res2 = new JStringBuilder
    var i = 0
    val len = s.length
    while(i < len) {
      f(s.charAt(i)) match {
        case Left(c) => res1.append(c)
        case Right(c) => res2.append(c)
      }
      i += 1
    }
    (res1.toString, res2.toString)
  }

  /** Analogous to `zip` except that the elements in each collection are not consumed until a strict operation is
    * invoked on the returned `LazyZip2` decorator.
    *
    * Calls to `lazyZip` can be chained to support higher arities (up to 4) without incurring the expense of
    * constructing and deconstructing intermediary tuples.
    *
    * {{{
    *    val xs = List(1, 2, 3)
    *    val res = (xs lazyZip xs lazyZip xs lazyZip xs).map((a, b, c, d) => a + b + c + d)
    *    // res == List(4, 8, 12)
    * }}}
    *
    * @param that the iterable providing the second element of each eventual pair
    * @tparam B   the type of the second element in each eventual pair
    * @return a decorator `LazyZip2` that allows strict operations to be performed on the lazily evaluated pairs
    *         or chained calls to `lazyZip`. Implicit conversion to `Iterable[(A, B)]` is also supported.
    */
  def lazyZip[B](that: Iterable[B]): LazyZip2[Char, B, String] = new LazyZip2(s, new WrappedString(s), that)


  /* ************************************************************************************************************
     The remaining methods are provided for completeness but they delegate to WrappedString implementations which
     may not provide the best possible performance. We need them in `StringOps` because their return type
     mentions `C` (which is `String` in `StringOps` and `WrappedString` in `WrappedString`).
     ************************************************************************************************************ */


  /** Computes the multiset difference between this string and another sequence.
    *
    *  @param that   the sequence of chars to remove
    *  @return       a new string which contains all chars of this string
    *                except some of occurrences of elements that also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *                part of the result, but any following occurrences will.
    *  @note         $unicodeunaware
    */
  def diff[B >: Char](that: Seq[B]): String = new WrappedString(s).diff(that).unwrap

  /** Computes the multiset intersection between this string and another sequence.
    *
    *  @param that   the sequence of chars to intersect with.
    *  @return       a new string which contains all chars of this string
    *                which also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *                in the result, but any following occurrences will be omitted.
    * @note          $unicodeunaware
    */
  def intersect[B >: Char](that: Seq[B]): String = new WrappedString(s).intersect(that).unwrap

  /** Selects all distinct chars of this string ignoring the duplicates.
    *
    * @note $unicodeunaware
    */
  def distinct: String = new WrappedString(s).distinct.unwrap

  /** Selects all distinct chars of this string ignoring the duplicates as determined by `==` after applying
    * the transforming function `f`.
    *
    * @param f The transforming function whose result is used to determine the uniqueness of each element
    * @tparam B the type of the elements after being transformed by `f`
    * @return a new string consisting of all the chars of this string without duplicates.
    * @note $unicodeunaware
    */
  def distinctBy[B](f: Char => B): String = new WrappedString(s).distinctBy(f).unwrap

  /** Sorts the characters of this string according to an Ordering.
    *
    *  The sort is stable. That is, elements that are equal (as determined by
    *  `ord.compare`) appear in the same order in the sorted sequence as in the original.
    *
    *  @see [[scala.math.Ordering]]
    *
    *  @param  ord the ordering to be used to compare elements.
    *  @return     a string consisting of the chars of this string
    *              sorted according to the ordering `ord`.
    *  @note       $unicodeunaware
    */
  def sorted[B >: Char](implicit ord: Ordering[B]): String = new WrappedString(s).sorted(ord).unwrap

  /** Sorts this string according to a comparison function.
    *
    *  The sort is stable. That is, elements that are equal (as determined by
    *  `lt`) appear in the same order in the sorted sequence as in the original.
    *
    *  @param  lt  the comparison function which tests whether
    *              its first argument precedes its second argument in
    *              the desired ordering.
    *  @return     a string consisting of the elements of this string
    *              sorted according to the comparison function `lt`.
    *  @note       $unicodeunaware
    */
  def sortWith(lt: (Char, Char) => Boolean): String = new WrappedString(s).sortWith(lt).unwrap

  /** Sorts this string according to the Ordering which results from transforming
    * an implicitly given Ordering with a transformation function.
    *
    * The sort is stable. That is, elements that are equal (as determined by
    * `ord.compare`) appear in the same order in the sorted sequence as in the original.
    *
    *  @see [[scala.math.Ordering]]
    *  @param   f the transformation function mapping elements
    *           to some other domain `B`.
    *  @param   ord the ordering assumed on domain `B`.
    *  @tparam  B the target type of the transformation `f`, and the type where
    *           the ordering `ord` is defined.
    *  @return  a string consisting of the chars of this string
    *           sorted according to the ordering where `x < y` if
    *           `ord.lt(f(x), f(y))`.
    *  @note    $unicodeunaware
    */
  def sortBy[B](f: Char => B)(implicit ord: Ordering[B]): String = new WrappedString(s).sortBy(f)(ord).unwrap

  /** Partitions this string into a map of strings according to some discriminator function.
    *
    *  @param f     the discriminator function.
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @return      A map from keys to strings such that the following invariant holds:
    *               {{{
    *                 (xs groupBy f)(k) = xs filter (x => f(x) == k)
    *               }}}
    *               That is, every key `k` is bound to a string of those elements `x`
    *               for which `f(x)` equals `k`.
    *  @note        $unicodeunaware
    */
  def groupBy[K](f: Char => K): immutable.Map[K, String] = new WrappedString(s).groupBy(f).view.mapValues(_.unwrap).toMap

  /** Groups chars in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in grouped.)
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of chars per group
    *  @param step the distance between the first chars of successive groups
    *  @return An iterator producing strings of size `size`, except the
    *          last element (which may be the only element) will be truncated
    *          if there are fewer than `size` chars remaining to be grouped.
    * @note    $unicodeunaware
    */
  def sliding(size: Int, step: Int = 1): Iterator[String] = new WrappedString(s).sliding(size, step).map(_.unwrap)

  /** Iterates over combinations of elements.
   *
   *  A '''combination''' of length `n` is a sequence of `n` elements selected in order of their first index in this sequence.
   *
   *  For example, `"xyx"` has two combinations of length 2. The `x` is selected first: `"xx"`, `"xy"`.
   *  The sequence `"yx"` is not returned as a combination because it is subsumed by `"xy"`.
   *
   *  If there is more than one way to generate the same combination, only one will be returned.
   *
   *  For example, the result `"xy"` arbitrarily selected one of the `x` elements.
   *
   *  As a further illustration, `"xyxx"` has three different ways to generate `"xy"` because there are three elements `x`
   *  to choose from. Moreover, there are three unordered pairs `"xx"` but only one is returned.
   *
   *  It is not specified which of these equal combinations is returned. It is an implementation detail
   *  that should not be relied on. For example, the combination `"xx"` does not necessarily contain
   *  the first `x` in this sequence. This behavior is observable if the elements compare equal
   *  but are not identical.
   *
   *  As a consequence, `"xyx".combinations(3).next()` is `"xxy"`: the combination does not reflect the order
   *  of the original sequence, but the order in which elements were selected, by "first index";
   *  the order of each `x` element is also arbitrary.
   *
   *  @return   An Iterator which traverses the n-element combinations of this string.
   *  @example {{{
   *    "abbbc".combinations(2).foreach(println)
   *    // ab
   *    // ac
   *    // bb
   *    // bc
   *    "bab".combinations(2).foreach(println)
   *    // bb
   *    // ba
   *  }}}
   *  @note     $unicodeunaware
   */
  def combinations(n: Int): Iterator[String] = new WrappedString(s).combinations(n).map(_.unwrap)

  /** Iterates over distinct permutations of elements.
   *
   *  @return   An Iterator which traverses the distinct permutations of this string.
   *  @example {{{
   *    "abb".permutations.foreach(println)
   *    // abb
   *    // bab
   *    // bba
   *  }}}
   *  @note     $unicodeunaware
   */
  def permutations: Iterator[String] = new WrappedString(s).permutations.map(_.unwrap)
}

final case class StringView(s: String) extends AbstractIndexedSeqView[Char] {
  def length = s.length
  @throws[StringIndexOutOfBoundsException]
  def apply(n: Int) = s.charAt(n)
  override def toString: String = s"StringView($s)"
}
