/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import collection.immutable.Vector
import collection.generic.VectorView

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
 *  @author Martin Odersky
 *  @version 2.8
 */
class Range(val start: Int, val end: Int, val step: Int) extends VectorView[Int, Vector[Int]] {
  require(step != 0)

  protected def underlying = Vector.empty[Int]

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(step: Int): Range = new Range(start, end, step)

  override def foreach[U](f: Int => U) {
    var i = start
    if (step > 0) {
      while (i < end) {
        f(i)
        i += step
      }
    } else {
      while (i > end) {
        f(i)
        i += step
      }
    }
  }

  lazy val length: Int = {
    def plen(start: Int, end: Int, step: Int) =
      if (end <= start) 0 else (end - start - 1) / step + 1
    if (step > 0) plen(start, end, step)
    else plen(end, start, -step)
  }

  def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  def contains(x: Int): Boolean =
    if (step > 0) start <= x && x < end
    else start >= x && x > end

  def inclusive = Range.inclusive(start, end, step)
}
object Range {
  /** @deprecated use Range.inclusive instead */
  class Inclusive(start: Int, end0: Int, step: Int)
      extends Range(start, if (step > 0) end0 + 1 else end0 - 1, step) { self =>
    override def by(step: Int): Range = new Inclusive(start, end0, step)
  }

  def apply(start: Int, end: Int, step: Int) =
    new Range(start, end, step)

  def inclusive(start: Int, end: Int, step: Int): Range =
    new Range.Inclusive(start, end, step)
}
