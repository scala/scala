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

/** A template for companion objects of `SortedMap` and subclasses thereof.
 *
 *  @since 2.8
 *  @define Coll `SortedMap`
 *  @define coll sorted map
 *  @define factoryInfo
 *    This object provides a set of operations needed to create sorted maps of type `$Coll`.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define sortedMapCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for sorted maps
 */
abstract class ImmutableSortedMapFactory[CC[A, B] <: immutable.SortedMap[A, B] with SortedMapLike[A, B, CC[A, B]]] extends SortedMapFactory[CC]
