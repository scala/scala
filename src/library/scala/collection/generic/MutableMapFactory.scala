/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class MutableMapFactory[CC[A, B] <: mutable.Map[A, B] with MutableMapTemplate[A, B, CC[A, B]]]
  extends MapFactory[CC] {

  def newBuilder[A, B] = new MapBuilder[A, B, CC[A, B]](empty[A, B])
}
