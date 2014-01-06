/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package generic

import scala.language.higherKinds

/** A template for companion objects of `immutable.Map` and subclasses thereof.
 *    @author Martin Odersky
 *    @version 2.8
 *    @since 2.8
 */
abstract class ImmutableMapFactory[CC[A, +B] <: immutable.Map[A, B] with immutable.MapLike[A, B, CC[A, B]]] extends MapFactory[CC]
