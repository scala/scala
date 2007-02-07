/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichLong(x: Long) extends Proxy with Ordered[Long] {

  // Proxy.self
  def self: Any = x

  // Ordered[Long].compare
  def compare (y: Long): Int = if (x < y) -1 else if (x > y) 1 else 0

  def min(y: Long): Long = if (x < y) x else y
  def max(y: Long): Long = if (x > y) x else y
  def abs: Long = if (x < 0) -x else x

}
