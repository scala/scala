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
class Range(val start: Int, val end: Int, val step: Int) extends RandomAccessSeq.Projection[Int] {
  assert(step != 0)
  assert(if (step > 0) end >= start else end <= start)

  override def length = {
    val base = if (start < end) end - start
               else start - end
    assert(base >= 0)
    val step = if (this.step < 0) -this.step else this.step
    assert(step >= 0)
    base / step + (if (base % step != 0) 1 else 0)
  }

  override def apply(idx : Int) = {
    if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
    start + (step * idx)
  }
  override protected def stringPrefix = "Range"

  def contains(x : Int): Boolean = {
    x >= start && x < end && (((x - start) % step) == 0)
  }
  override def contains(elem: Any): Boolean = elem match {
  case elem : Int => contains(elem)
  case _ => false
  }
}
