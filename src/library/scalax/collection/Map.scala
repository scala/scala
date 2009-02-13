/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scalax.collection

import generic._

trait Map[A, B] extends MapTemplate[A, B, Map]

/* Factory object for `Map` class */
object Map extends MapFactory[Map] {
  def empty[A, B]: Map[A, B] = new immutable.EmptyMap[A, B]
}
