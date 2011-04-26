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
abstract class GenMapFactory[CC[A, B] <: GenMap[A, B] with GenMapLike[A, B, CC[A, B]]] {

  /** The type constructor of the collection that can be built by this factory */
  type Coll = CC[_, _]

  /** An empty $Coll */
  def empty[A, B]: CC[A, B]

  /** A collection of type $Coll that contains given key/value bindings.
   *  @param elems   the key/value pairs that make up the $coll
   *  @tparam A      the type of the keys
   *  @tparam B      the type of the associated values
   *  @return        a new $coll consisting key/value pairs given by `elems`.
   */
  def apply[A, B](elems: (A, B)*): CC[A, B] = (newBuilder[A, B] ++= elems).result

  /** The default builder for $Coll objects.
   *  @tparam A      the type of the keys
   *  @tparam B      the type of the associated values
   */
  def newBuilder[A, B]: Builder[(A, B), CC[A, B]] = new MapBuilder[A, B, CC[A, B]](empty[A, B])

  /** The standard `CanBuildFrom` class for maps.
   */
  class MapCanBuildFrom[A, B] extends CanBuildFrom[Coll, (A, B), CC[A, B]] {
    def apply(from: Coll) = newBuilder[A, B]
    def apply() = newBuilder
  }
}
