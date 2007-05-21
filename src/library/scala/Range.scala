/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala

import Predef._

/** <p>
 *    The <code>Range</code> class represents integer values in range
 *    <code>[start;end)</code> with non-zero step value <code>step</code>.
 *    For example:
 *  </p><pre>
 *     <b>val</b> r1 = Iterator.range(0, 10)
 *     <b>val</b> r2 = Iterator.range(r1.start, r1.end, r1.step + 1)
 *     println(r2.length) // = 5
 *  </pre>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 01/05/2007
 */
class Range(val start: Int, val end: Int, val step: Int) extends BufferedIterator[Int] {
  assert(step != 0)
  assert(if (step > 0) end >= start else end <= start)
  private var i = start

  override def hasNext: Boolean = if (step > 0) i < end else i > end

  def next: Int =
    if (hasNext) { val j = i; i += step; j }
    else throw new NoSuchElementException("next on empty iterator")

  def peekList(sz : Int) = new RandomAccessSeq[Int] {
    def length = Math.min(sz, length0(i));
    def apply(idx : Int) = {
      if (idx >= length) throw new IndexOutOfBoundsException
      i + (idx * step)
    }
  }
  protected override def defaultPeek : Int = throw new NoSuchElementException

  private def length0(i : Int) = {
    if (step > 0) length1(i, end, step)
    else length1(end, i, -step)
  }
  private def length1(start : Int, end : Int, step : Int) = {
    assert(start <= end && step > 0)
    val n = (end - start) / step
    val m = (end - start) % step
    n + (if (m == 0) 0 else 1)
  }
  def length: Int = length0(start)

  def contains(x: Int): Boolean =
    Iterator.range(0, length) exists (i => x == start + i * step)

}
