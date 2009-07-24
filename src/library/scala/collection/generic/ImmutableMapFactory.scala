/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.generic
import scala.collection._

/** A template for companion objects of <code>immutable.Map</code> and
 *  subclasses thereof.
 */
abstract class ImmutableMapFactory[CC[A, +B] <: immutable.Map[A, B] with ImmutableMapTemplate[A, B, CC[A, B]]]
  extends MapFactory[CC] {

  def newBuilder[A, B] = new MapBuilder[A, B, CC[A, B]](empty[A, B])
}
