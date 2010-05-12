/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime

import collection.immutable.Range


final class RichInt(val start: Int) extends Proxy with Ordered[Int] {

  // Proxy
  def self: Any = start

  // Ordered[Int]
  def compare(that: Int): Int = if (start < that) -1 else if (start > that) 1 else 0

  def until(end: Int): Range with Range.ByOne = Range(start, end)
  def until(end: Int, step: Int): Range = Range(start, end, step)

  /** like <code>until</code>, but includes the last index */
  def to(end: Int): Range.Inclusive with Range.ByOne = Range.inclusive(start, end)
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(start, end, step)

  def min(that: Int): Int = if (start < that) start else that
  def max(that: Int): Int = if (start > that) start else that
  def abs: Int = if (start < 0) -start else start

  def toBinaryString: String = java.lang.Integer.toBinaryString(start)
  def toHexString: String = java.lang.Integer.toHexString(start)
  def toOctalString: String = java.lang.Integer.toOctalString(start)
}
