/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import java.io._
import util.matching.Regex


/** The CharSequence object defines variance implementations of character sequences
 */
object LazyCharSequence {
  final val UndeterminedEnd = Math.MAX_INT

  /** Constructs a character sequence from a character iterator */
  def fromChars(source: Iterator[Char]): CharSequence =
    new LazyCharSequence ((chars: Array[Char], start: Int, len: Int) => {
      var i = 0
      while (i < len && source.hasNext) {
        chars(start + i) = source.next
        i += 1
      }
      if (i == 0) -1 else i
    })

  /** Constructs a character sequence from a character iterable */
  def fromChars(source: Iterable[Char]): CharSequence =
    fromChars(source.elements)

  /** Constructs a character sequence from a string iterator */
  def fromStrings(source: Iterator[String]): CharSequence = {
    var current: String = ""
    def more(chars: Array[Char], start: Int, len: Int): Int =
      if (current.length != 0) {
        val nchars = current.length min len
        current.getChars(0, nchars, chars, start)
        current = current.substring(nchars)
        if (nchars == len) nchars
        else (more(chars, start + nchars, len - nchars) max 0) + nchars
      } else if (source.hasNext) {
        current = source.next
        more(chars, start, len)
      } else -1
    new LazyCharSequence(more(_: Array[Char], _: Int, _: Int))
  }

  /** Constructs a character sequence from a string iterable */
  def fromStrings(source: Iterable[String]): CharSequence =
    fromStrings(source.elements)

  /** Constructs a character sequence from a line iterator
   *  Lines do not contain trailing `\n' characters; The method inserts
   *  a line separator `\n' between any two lines in the sequence.
   */
  def fromLines(source: Iterator[String]): CharSequence = {
    var isFirst = true
    fromStrings(source map { line =>
      if (isFirst) line
      else {
        isFirst = false
        "\n"+line
      }
    })
  }

  /** Constructs a character sequence from a line iterable
   *  Lines do not contain trailing `\n' characters; The method inserts
   *  a line separator `\n' between any two lines in the sequence.
   */
  def fromLines(source: Iterable[String]): CharSequence =
    fromLines(source.elements)

  /** Constructs a character sequence from an input reader
   */
  def fromReader(source: Reader): CharSequence =
    new LazyCharSequence(source)

  /** Constructs a character sequence from an input file
   */
  def fromFile(source: File) =
    new LazyCharSequence(source)

  /** Constructs a character sequence from a file with given name
   */
  def fromFile(source: String) =
    new LazyCharSequence(source)

  /** Constructs a character sequence from a scala.io.Source value
   */
  def fromSource(source: io.Source) =
    fromLines(source.getLines)
}


import LazyCharSequence._

/** An implementation of lazily computed character sequences
 *
 * @author Martin Odersky
 */
class LazyCharSequence protected (more: (Array[Char], Int, Int) => Int,
                                  first: Page, start: Int, end: Int) extends CharSequence {

  /** Constructs a character sequence from a method that produces more characters when asked.
   *  The producer method is analogous to the read method in java.io.Reader.
   *  It takes three parameters: an array of characters, a start index, and an end index.
   *  It should try to fill the array between start and end indices (not including end index).
   *  It returns the number of characters produced, or -1 if end of logical input stream was reached
   *  before any character was read.
   */
  def this(more: (Array[Char], Int, Int) => Int) = this(more, new Page(0), 0, UndeterminedEnd)

  /** Constructs a character sequence from an input reader
   */
  def this(source: Reader) =
    this(source.read(_: Array[Char], _: Int, _: Int))

  /** Constructs a character sequence from an input file
   */
  def this(source: File) =
    this(new FileReader(source))

  /** Constructs a character sequence from a file with given name
   */
  def this(source: String) =
    this(new File(source))

  private var current: Page = first

  private def latest = first.latest

  private def addMore() = latest.addMore(more)

  private def page(absindex: Int) = {
    if (absindex < current.start)
      current = first
    while (absindex >= current.end && current.next != null)
      current = current.next
    while (absindex >= current.end && !current.isLast) {
      current = addMore()
    }
    current
  }

  /** The length of the character sequence
   *  Note: calling this method will force sequence to be read until the end.
   */
  def length: Int = {
    while (!latest.isLast) addMore()
    (latest.end min end) - start
  }

  /** The character at position `index'.
   */
  def charAt(index: Int) =
    if (isDefinedAt(index)) page(index + start)(index + start)
    else throw new IndexOutOfBoundsException(index.toString)

  /** Is character sequence defined at `index'?
   *  Unlike `length' this operation does not force reading
   *  a lazy sequence to the end.
   */
  override def isDefinedAt(index: Int) =
    index >= 0 && index < end - start && {
      val p = page(index + start); index + start < p.end
    }

  /** Optionally the character at position `index'. None is not in range.
   */
  override def get(index: Int) =
    if (isDefinedAt(index)) Some(page(index + start)(index + start))
    else None

  /** the subsequence from index `start' up to and excluding
   *  the minimum of index `end' and the length of current sequence.
   */
  def subSequence(_start: Int, _end: Int) = {
    page(start)
    val s = start + _start
    val e = if (_end == UndeterminedEnd) _end else start + _end
    var f = first
    while (f.end <= s && !f.isLast) f = f.next
    new LazyCharSequence(more, f, s, e)
  }

  /** The subsequence from index `start' until the end of the current sequence
   *  This operation does not force reading to the end.
   */
  override def subSequence(start: Int): CharSequence =
    subSequence(start, UndeterminedEnd)

  /** Convert sequence to string */
  override def toString = {
    val buf = new StringBuilder
    for (ch <- elements) buf append ch
    buf.toString
  }
}


/** Page containing up to PageSize characters of the input sequence.
 */
private class Page(val num: Int) {

  private final val PageSize = 4096

  /** The next page in the sequence */
  var next  : Page = null

  /** A later page in the sequence, serves a cachae for pointing to last page */
  var later : Page = this

  /** The number of characters read into this page */
  var filled: Int = 0

  /** Is this page the permamnently last one in the sequence? Only true once `more'
   *  method has returned -1 to signal end of input. */
  var isLast: Boolean = false

  /** The character array */
  final val chars = new Array[Char](PageSize)

  /** The index of the first character in this page relative to the whole sequence */
  final def start = num * PageSize

  /** The index of the character following the last charcater in this page relative
   *  to the whole sequence */
  final def end = start + filled

  /** The currently last page in the sequence; might change as more charcaters are appended */
  final def latest: Page = {
    if (later.next != null) later = later.next.latest
    later
  }

  /** The character at given sequence index.
   *  That index is relative to the whole sequence, not the page. */
  def apply(index: Int) = {
    if (index < start || index - start >= filled) throw new IndexOutOfBoundsException(index.toString)
    chars(index - start)
  }

  /** produces more characters by calling `more' and appends them on the current page,
   *  or fills a subsequent page if current page is full
   *  pre: if current page is full, it is the last one in the sequence.
   */
  final def addMore(more: (Array[Char], Int, Int) => Int): Page =
    if (filled == PageSize) {
      next = new Page(num + 1)
      next.addMore(more)
    } else {
      val count = more(chars, filled, PageSize - filled)
      if (count < 0) isLast = true
      else filled += count
      this
    }
}
