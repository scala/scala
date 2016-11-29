/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

import java.io.{File, FileReader, Reader}
import scala.reflect.ClassTag

/** The `PagedSeq` object defines a lazy implementations of
 *  a random access sequence.
 *
 * Provides utility methods that return instances of `PagedSeq[Char]`.
 * `fromIterator` and `fromIterable` provide generalised instances of `PagedSeq`
 *  @since 2.7
 */
@deprecated("this object will be moved to the scala-parser-combinators module", "2.11.8")
object PagedSeq {
  final val UndeterminedEnd = Int.MaxValue

  /** Constructs a paged sequence from an iterator */
  def fromIterator[T: ClassTag](source: Iterator[T]): PagedSeq[T] =
    new PagedSeq[T]((data: Array[T], start: Int, len: Int) => {
      var i = 0
      while (i < len && source.hasNext) {
        data(start + i) = source.next()
        i += 1
      }
      if (i == 0) -1 else i
    })

  /** Constructs a paged sequence from an iterable */
  def fromIterable[T: ClassTag](source: Iterable[T]): PagedSeq[T] =
    fromIterator(source.iterator)

  /** Constructs a paged character sequence from a string iterator */
  def fromStrings(source: Iterator[String]): PagedSeq[Char] = {
    var current: String = ""
    def more(data: Array[Char], start: Int, len: Int): Int =
      if (current.length != 0) {
        val cnt = current.length min len
        current.getChars(0, cnt, data, start)
        current = current.substring(cnt)
        if (cnt == len) cnt
        else (more(data, start + cnt, len - cnt) max 0) + cnt
      } else if (source.hasNext) {
        current = source.next()
        more(data, start, len)
      } else -1
    new PagedSeq(more(_: Array[Char], _: Int, _: Int))
  }

  /** Constructs a paged character sequence from a string iterable */
  def fromStrings(source: Iterable[String]): PagedSeq[Char] =
    fromStrings(source.iterator)

  /** Constructs a paged character sequence from a line iterator
   *  Lines do not contain trailing `\n` characters; The method inserts
   *  a line separator `\n` between any two lines in the sequence.
   */
  def fromLines(source: Iterator[String]): PagedSeq[Char] = {
    var isFirst = true
    fromStrings(source map { line =>
      if (isFirst) {
        isFirst = false
        line
      } else "\n"+line
    })
  }

  /** Constructs a paged character sequence from a line iterable
   *  Lines do not contain trailing `\n` characters; The method inserts
   *  a line separator `\n` between any two lines in the sequence.
   */
  def fromLines(source: Iterable[String]): PagedSeq[Char] =
    fromLines(source.iterator)

  /** Constructs a paged character sequence from an input reader
   */
  def fromReader(source: Reader): PagedSeq[Char] =
    new PagedSeq(source.read(_: Array[Char], _: Int, _: Int))

  /** Constructs a paged character sequence from an input file
   */
  def fromFile(source: File): PagedSeq[Char] =
    fromReader(new FileReader(source))

  /** Constructs a paged character sequence from a file with given name
   */
  def fromFile(source: String): PagedSeq[Char] =
    fromFile(new File(source))

  /** Constructs a paged character sequence from a scala.io.Source value
   */
  def fromSource(source: scala.io.Source) =
    fromLines(source.getLines())
}


import PagedSeq._

/** An implementation of lazily computed sequences, where elements are stored
 *  in "pages", i.e. arrays of fixed size.
 *
 *  A paged sequence is constructed from a function that produces more elements when asked.
 *  The producer function - `more`, is similar to the read method in java.io.Reader.
 *  The `more` function takes three parameters: an array of elements, a start index, and an end index.
 *  It should try to fill the array between start and end indices (excluding end index).
 *  It returns the number of elements produced, or -1 if end of logical input stream was reached
 *  before reading any element.
 *
 *  @tparam T     the type of the elements contained in this paged sequence, with an `ClassTag` context bound.
 *
 *  @author Martin Odersky
 *  @since  2.7
 *  @define Coll `PagedSeq`
 *  @define coll paged sequence
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@deprecated("this class will be moved to the scala-parser-combinators module", "2.11.8")
class PagedSeq[T: ClassTag] protected(
  more: (Array[T], Int, Int) => Int,
  first1: Page[T],
  start: Int,
  end: Int)
extends scala.collection.AbstractSeq[T]
   with scala.collection.IndexedSeq[T]
{
  def this(more: (Array[T], Int, Int) => Int) = this(more, new Page[T](0), 0, UndeterminedEnd)

  private var current: Page[T] = first1

  private def latest = first1.latest

  private def addMore() = latest.addMore(more)

  private def page(absindex: Int) = {
    if (absindex < current.start)
      current = first1
    while (absindex >= current.end && current.next != null)
      current = current.next
    while (absindex >= current.end && !current.isLast) {
      current = addMore()
    }
    current
  }

  /** The length of the paged sequence
   *  @note Calling this method will force the entire sequence to be read.
   */
  def length: Int = {
    while (!latest.isLast && latest.end < end) addMore()
    (latest.end min end) - start
  }

  /** The element at position `index`.
   */
  def apply(index: Int) =
    if (isDefinedAt(index)) page(index + start)(index + start)
    else throw new IndexOutOfBoundsException(index.toString)

  /** Predicate method to check if an element is defined
   *  at position `index` of the current sequence.
   *  Unlike `length` this operation does not force reading
   *  a lazy sequence to the end.
   */
  override def isDefinedAt(index: Int) =
    index >= 0 && index < end - start && {
      val absidx = index + start
      absidx >= 0 && absidx < page(absidx).end
    }

   /** The subsequence from index `start` up to `end -1` if `end`
   *   is lesser than the length of the current sequence and up to
   *   length of the sequence otherwise. This is limited up to the length
   *   of the current sequence if `end` is larger than its length.
   */
  override def slice(_start: Int, _end: Int): PagedSeq[T] = {
    page(start)
    val s = start + _start
    val e = if (_end == UndeterminedEnd) _end else start + _end
    var f = first1
    while (f.end <= s && !f.isLast) {
      if (f.next eq null) f = f.addMore(more)
      else f = f.next
    }
    // Warning -- not refining `more` means that slices can freely request and obtain
    // data outside of their slice.  This is part of the design of PagedSeq
    // (to read pages!) but can be surprising.
    new PagedSeq(more, f, s, e)
  }

  /** The subsequence from index `start` up to
   *  the length of the current sequence.
   */
  def slice(start: Int): PagedSeq[T] = slice(start, UndeterminedEnd)

  /** Convert sequence to string */
  override def toString = {
    val buf = new StringBuilder
    for (ch <- PagedSeq.this.iterator) buf append ch
    buf.toString
  }
}


/** Page containing up to PageSize characters of the input sequence.
 */
private class Page[T: ClassTag](val num: Int) {

  private final val PageSize = 4096

  /** The next page in the sequence */
  var next  : Page[T] = null

  /** A later page in the sequence, serves a cache for pointing to last page */
  var later : Page[T] = this

  /** The number of elements read into this page */
  var filled: Int = 0

  /** Set true if the current page is the last in the sequence or if
  *   the `more` function returned -1 signalling end of input. */
  var isLast: Boolean = false

  /** The element array */
  final val data = new Array[T](PageSize)

  /** The index of the first element in this page relative to the whole sequence */
  final def start = num * PageSize

  /** The index of the element following the last element in this page relative
   *  to the whole sequence */
  final def end = start + filled

  /** The last page as currently present in the sequence; This can change as more
   *  elements get appended to the sequence.  */
  final def latest: Page[T] = {
    if (later.next != null) later = later.next.latest
    later
  }

  /** The element at the given sequence index.
   *  That index is relative to the whole sequence, not the page. */
  def apply(index: Int) = {
    if (index < start || index - start >= filled) throw new IndexOutOfBoundsException(index.toString)
    data(index - start)
  }

  /** Produces more elements by calling `more` and adds them on the current page,
   *  or fills a subsequent page if current page is full.
   *  @note If current page is full, it is the last one in the sequence.  */
  final def addMore(more: (Array[T], Int, Int) => Int): Page[T] =
    if (filled == PageSize) {
      next = new Page[T](num + 1)
      next.addMore(more)
    } else {
      val count = more(data, filled, PageSize - filled)
      if (count < 0) isLast = true
      else filled += count
      this
    }
}
