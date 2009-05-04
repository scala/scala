/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/**
 *  @author Stephane Micheloud
 *
 *  @deprecated Use class <code>scala.util.Random</code> instead.
 */
@deprecated
class Random(self: java.util.Random) extends util.Random(self) {
  def this(seed: Long) = this(new java.util.Random(seed))
  def this(seed: Int) = this(seed.toLong)
  def this() = this(new java.util.Random())
}

