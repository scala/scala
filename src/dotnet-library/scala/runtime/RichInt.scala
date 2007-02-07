/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime

final class RichInt(x: Int) extends Proxy with Ordered[Int] {

  // Proxy
  def self: Any = x

  // Ordered[Int]
  def compare (y: Int): Int = if (x < y) -1 else if (x > y) 1 else 0

  def until(y: Int): Iterator[Int] = Iterator.range(x, y)
  def to(y: Int): Iterator[Int] = Iterator.range(x, y + 1)

  def min(y: Int): Int = if (x < y) x else y
  def max(y: Int): Int = if (x > y) x else y
  def abs: Int = if (x < 0) -x else x

}
