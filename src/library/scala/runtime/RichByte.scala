/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichByte(x: Byte) extends Proxy with Ordered[Byte] {

  // Proxy.self
  def self: Any = x

  // Ordered[Byte].compare
  def compare (y: Byte): Int = if (x < y) -1 else if (x > y) 1 else 0

}
