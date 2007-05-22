/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime

final class RichInt(start: Int) extends Proxy with Ordered[Int] {

  // Proxy
  def self: Any = start

  // Ordered[Int]
  def compare (that: Int): Int = if (start < that) -1 else if (start > that) 1 else 0

  /** Creates an iteration of integers from <code>this</code> until <code>end</code>,
      incrementing or decrementing by 1 as appropriate. */
  def until(end: Int): Range = until(end, +1)
  def until(end : Int, step : Int) : Range = {
    if (start <= end && step > 0) new Range(start, end, step)
    else if (start >= end && step < 0) new Range(start, end, step)
    else if (start >= end && step > 0) new Range(start, end, -step)
    else throw new Predef.IllegalArgumentException("" + start + " until " + end + " with step " + step)
  }

  def to(end: Int): Range = {
    if (start == end) until(start)
    else if (start < end) until(end + 1)
    else until(end - 1)
  }

  def min(that: Int): Int = if (start < that) start else that
  def max(that: Int): Int = if (start > that) start else that
  def abs: Int = if (start < 0) -start else start

}
