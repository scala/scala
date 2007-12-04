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
class Range(val start: Int, val end: Int, val step: Int) extends RandomAccessSeq.Projection[Int] {
  if (step == 0) throw new Predef.IllegalArgumentException

  /** create a new range with the start and end values of this range and a new <code>step</code> */
  def by(step : Int) : Range = this match {
    case inclusive : Range.Inclusive => new Range.Inclusive(start, end, step)
    case _ => new Range(start, end, step)
  }

  lazy val length : Int = {
    if (start < end && this.step < 0) 0
    else if (start > end && this.step > 0) 0
    else {
      val base = if (start < end) end - start
                 else start - end
      assert(base >= 0)
      val step = if (this.step < 0) -this.step else this.step
      assert(step >= 0)
      base / step + last(base, step)
    }
  }
  protected def last(base : Int, step : Int) = (if (base % step != 0) 1 else 0)
  def apply(idx : Int) : Int = {
    if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
    start + (step * idx)
  }
  /** a <code>Seq.contains</code>, not a <code>Iterator.contains</code>! */
  def contains(x : Int): Boolean =
    x >= start && x < end && (((x - start) % step) == 0)
  def inclusive = new Range.Inclusive(start,end,step)
}
object Range {
  class Inclusive(start : Int, end : Int, by : Int) extends Range(start,end,by) {
    override def apply(idx : Int) : Int = super.apply(idx)
    override protected def last(base : Int, step : Int) : Int = 1
    // XXX: breaks scala doc!
    // override def by(step : Int) : Range = new Inclusive(start, end, step)
  }
}

