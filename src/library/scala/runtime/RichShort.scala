/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichShort(start: Short) extends Proxy with Ordered[Short] {

  // Proxy.self
  def self: Any = start

  // Ordered[Short].compare
  def compare(that: Short): Int = if (start < that) -1 else if (start > that) 1 else 0

}
