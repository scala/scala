/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:


package scala

import java.io._
import collection._
import util.matching.Regex

/** A Scala abstraction of character sequences, similar to, but richer than,
 *  java.lang.CharSequence. Sequences can be fixed or lazy. A lazy
 *  character sequence may be constructed only while it is read.
 *
 * @author Martin Odersky
 */
trait CharSequence extends java.lang.CharSequence with Seq[Char] {

  /** The length of the character sequence
   *  Note: if char sequence is lazy, calling this method
   *  will force sequence to be read until the end.
   */
  def length: Int

  /** The character at position `index'.
   */
  def charAt(index: Int): Char

  /** Is character sequence defined at `index'?
   *  Unlike `length' this operation does not force reading
   *  a lazy sequence to the end.
   *  (the implementation of that method is inherited from Seq).
   */
  def isDefinedAt(index: Int): Boolean

  /** Optionally the character at position `index'. None is not in range.
   */
  def get(index: Int): Option[Char] =
    if (isDefinedAt(index)) Some(charAt(index))
    else None

  /** The character at position `index'. (same as `charAt')
   */
  def apply(index: Int): Char = charAt(index)

  /** the subsequence from index `start' up to and excluding
   *  the minimum of index `end' and the length of current sequence.
   */
  def subSequence(start: Int, end: Int): CharSequence

  /** The subsequence from index `start' until the end of the current sequence
   *  If sequence is lazy, this operation does not force reading to the end.
   */
  def subSequence(start: Int): CharSequence =
    subSequence(start, length)

  /** Convert sequence to string */
  def toString: String

  /** the elements of this sequence */
  def elements: Iterator[Char] = new Iterator[Char] {
    private var index = 0
    def hasNext: Boolean = isDefinedAt(index)
    def next: Char = {
      val ch = charAt(index)
      index += 1
      ch
    }
  }

  /** Return all matches of given regexp in this character sequence as an iterator
   */
  def findAll(regex: Regex): Regex.MatchIterator = regex findAllIn this

  /** Return first match of given regexp in this character sequence as an optional value
   */
  def findFirst(regex: Regex): Option[String] = regex findFirstIn this

  /** Return first match of given regexp in this character sequence as an optional value
   */
  def findFirstMatch(regex: Regex): Option[Regex.Match] = regex findFirstMatchIn this

  /** Return optionally string matching given regexp at the beginning of this
   *  character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefix(regex: Regex): Option[String] = regex findPrefixOf this

  /** Return optionally match for given regexp at the beginning of this
   *  character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefixMatch(regex: Regex): Option[Regex.Match] = regex findPrefixMatchOf this

  /** Replaces all matches of given regexp by a string.
   *
   *  @param regex       The regex to match with
   *  @param replacement The string that will replace each match
   *  @return            The resulting string
   */
  def replaceAll(regex: Regex, replacement: String): String = regex replaceAllIn (this, replacement)

  /** Replaces the first match of given regexp by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace the match
   *  @return            The resulting string
   */
  def replaceFirst(regex: Regex, replacement: String): String = regex replaceFirstIn (this, replacement)

  def toArray: Array[Char] = {
    val len = length
    val result = new Array[Char](len)
    var i = 0
    while (i < len) {
      result(i) = charAt(i)
      i += 1
    }
    result
  }
}

/** The CharSequence object defines variance implementations of character sequences
 */
object CharSequence {

  def fromJava(source: java.lang.CharSequence): CharSequence =
    new JavaWrapper(source)

  def fromArray(source: Array[Char]): CharSequence =
    new AsArray(source, 0, source.length)
  def fromArray(source: Array[Char], start: Int): CharSequence =
    new AsArray(source, start, source.length)
  def fromArray(source: Array[Char], start: Int, end: Int): CharSequence =
    new AsArray(source, start, end)

  private class JavaWrapper(source: java.lang.CharSequence) extends CharSequence {
    def length: Int = source.length
    def charAt(index: Int): Char = source.charAt(index)
    def subSequence(start: Int, end: Int): CharSequence = new JavaWrapper(source.subSequence(start, end))
    override def toString: String = source.toString
  }

  private class AsArray(source: scala.Array[Char], start: Int, end: Int) extends CharSequence {
    if (start < 0) throw new IndexOutOfBoundsException
    if (end > source.length) throw new IndexOutOfBoundsException

    def charAt(index: Int) =
      if (start + index < end) source(index + start)
      else throw new IndexOutOfBoundsException

    def length: Int = if (end < start) 0 else end - start

    def subSequence(_start: Int, _end: Int) =
      new AsArray(source, start + _start, start + _end)

    override def toArray: Array[Char] = {
      val result = new Array[Char](length)
      compat.Platform.arraycopy(source, start, result, 0, length)
      result
    }

    override def toString = new String(source, start, end - start)
  }
}
