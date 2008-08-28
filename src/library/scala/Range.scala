/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import Predef._

/** <p>
 *    The <code>Range</code> class represents integer values in range
 *    <code>[start;end)</code> with non-zero step value <code>step</code>.
 *    Sort of acts like a sequence also (supports length and contains).
 *    For example:
 *  </p><pre>
 *     <b>val</b> r1 = 0 until 10
 *     <b>val</b> r2 = r1.start until r1.end by r1.step + 1
 *     println(r2.length) // = 5
 *  </pre>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 01/05/2007
 */
class Range(val start: Int, val end: Int, val step: Int) extends RandomAccessSeq.Projection[Int] {
  if (step == 0) throw new Predef.IllegalArgumentException

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(step: Int): Range = new Range(start, end, step)

  override def foreach(f: Int => Unit) {
    if (step > 0) {
      var i = this.start
      val until = if (inInterval(end)) end + 1 else end

      while (i < until) {
        f(i)
        i += step
      }
    } else {
      var i = this.start
      val until = if (inInterval(end)) end - 1 else end

      while (i > until) {
        f(i)
        i += step
      }
    }
  }

  lazy val length: Int = {
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

  protected def last(base: Int, step: Int): Int =
    if (base % step != 0) 1 else 0

  def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
    start + (step * idx)
  }

  /** a <code>Seq.contains</code>, not a <code>Iterator.contains</code>! */
  def contains(x: Int): Boolean = {
    inInterval(x) && (((x - start) % step) == 0)
  }

  /** Is the argument inside the interval defined by `start' and `end'?
   *  Returns true if `x' is inside [start, end).
   */
  protected def inInterval(x: Int): Boolean =
    if (step > 0)
      (x >= start && x < end)
    else
      (x <= start && x > end)

  def inclusive = new Range.Inclusive(start,end,step)
}

object Range {
  class Inclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    override def apply(idx: Int): Int = super.apply(idx)
    override protected def last(base: Int, step: Int): Int = 1
    override def by(step: Int): Range = new Inclusive(start, end, step)

    /** Returns true if x is inside the interval [start, end]. */
    override protected def inInterval(x: Int) =
      if (step > 0)
        (x >= start && x <= end)
      else
        (x <= start && x >= end)
  }
}
