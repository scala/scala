/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

/** A template for companion objects of `Map` and
 *  subclasses thereof.
 *
 *  @since 2.8
 *  @define Coll Map
 *  @define coll map
 *  @define factoryInfo
 *    This object provides a set of operations needed to create maps of type `$Coll`.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define mapCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for maps.
 */
abstract class ImmutableMapFactory[CC[A, +B] <: immutable.Map[A, B] with immutable.MapLike[A, B, CC[A, B]]] extends MapFactory[CC]
