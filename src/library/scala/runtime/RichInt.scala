/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import collection.immutable.Range


final class RichInt(val start: Int) extends Proxy with Ordered[Int] {

  // Proxy
  def self: Any = start

  // Ordered[Int]
  def compare(that: Int): Int = if (start < that) -1 else if (start > that) 1 else 0

  def until(end: Int, step: Int): Range = new Range(start, end, step)
  def until(end: Int): Range.ByOne = new Range.ByOne(start, end)

  /** like <code>until</code>, but includes the last index */
  def to(end: Int, step: Int): Range = Range.inclusive(start, end, step)
  def to(end: Int): Range.ByOne = new Range.ByOne(start, end + 1)

  def min(that: Int): Int = if (start < that) start else that
  def max(that: Int): Int = if (start > that) start else that
  def abs: Int = if (start < 0) -start else start

  def toBinaryString: String = java.lang.Integer.toBinaryString(start)
  def toHexString: String = java.lang.Integer.toHexString(start)
  def toOctalString: String = java.lang.Integer.toOctalString(start)
}
