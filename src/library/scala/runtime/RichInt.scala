/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
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

  def until(y: Int): Iterator[Int] = {
    if (y > x) Iterator.range(x, y, +1)
    else       Iterator.range(x, y, -1)
  }
  def to(y: Int): Iterator[Int] = until(y + 1)

  def min(y: Int): Int = if (x < y) x else y
  def max(y: Int): Int = if (x > y) x else y
  def abs: Int = if (x < 0) -x else x

}
