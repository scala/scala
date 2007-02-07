/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichShort(x: Short) extends Proxy with Ordered[Short] {

  // Proxy.self
  def self: Any = x

  // Ordered[Short].compare
  def compare (y: Short): Int = if (x < y) -1 else if (x > y) 1 else 0

}
