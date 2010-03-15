/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import JavaConversions._
import generic._


/**
 * @since 2.8
 */
class WeakHashMap[A, B] extends JMapWrapper[A, B](new java.util.WeakHashMap)
			   with JMapWrapperLike[A, B, WeakHashMap[A, B]] {
  override def empty = new WeakHashMap[A, B]
}

object WeakHashMap extends MutableMapFactory[WeakHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), WeakHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: WeakHashMap[A, B] = new WeakHashMap[A, B]
}

