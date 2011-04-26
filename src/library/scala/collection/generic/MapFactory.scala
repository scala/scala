/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic


import mutable.{Builder, MapBuilder}
import annotation.bridge

/** A template for companion objects of `Map` and subclasses thereof.
 *
 *  @define coll map
 *  @define Coll Map
 *  @define factoryInfo
 *    This object provides a set of operations needed to create `$Coll` values.
 *    @author Martin Odersky
 *    @version 2.8
 *    @since 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    @see CanBuildFrom
 *  @define mapCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for `$Coll` objects.
 *    The created value is an instance of class `MapCanBuildFrom`.
 *    @see CanBuildFrom
 *    @see GenericCanBuildFrom
 */
abstract class MapFactory[CC[A, B] <: Map[A, B] with MapLike[A, B, CC[A, B]]] extends GenMapFactory[CC] {

  def empty[A, B]: CC[A, B]

  @bridge
  override def apply[A, B](elems: (A, B)*): CC[A, B] = super.apply(elems: _*)
}
