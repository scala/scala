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
 *    Sort of acts like a sequence also (supports length and contains).
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
  private var jdx = 0

  override def peekList(sz : Int) : Seq[Int] = return new RandomAccessSeq.Projection[Int] {
    def length = Range.this.length - jdx
    def apply(idx : Int) = Range.this.apply(jdx + idx)
  }
  override def hasNext = jdx < length
  override def next = {
    val ret = apply(jdx)
    jdx = jdx + 1
    ret
  }
  def seqOfRange = new RandomAccessSeq.Projection[Int] {
    def length = Range.this.length
    def apply(idx : Int) = Range.this.apply(idx)
  }
  def length = {
    val base = if (start < end) end - start
               else start - end
    assert(base >= 0)
    val step = if (this.step < 0) -this.step else this.step
    assert(step >= 0)
    base / step + (if (base % step != 0) 1 else 0)
  }
  def apply(idx : Int) = {
    if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
    start + (step * idx)
  }
  /** a <code>Seq.contains</code>, not a <code>Iterator.contains</code>! */
  def contains(x : Int): Boolean =
    x >= start && x < end && (((x - start) % step) == 0)

  /** a <code>Iterator.contains</code>, not a <code>Seq.contains</code>! */
  override def contains(elem: Any): Boolean = super.contains(elem)
}
