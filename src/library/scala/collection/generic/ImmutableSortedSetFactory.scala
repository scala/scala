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

/** A template for companion objects of `SortedSet` and subclasses thereof.
 *
 *  @since 2.8
 *  @define Coll `immutable.SortedSet`
 *  @define coll immutable sorted set
 *  @define factoryInfo
 *    This object provides a set of operations needed to create sorted sets of type `$Coll`.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define sortedSetCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for sorted sets
 */
abstract class ImmutableSortedSetFactory[CC[A] <: immutable.SortedSet[A] with SortedSetLike[A, CC[A]]] extends SortedSetFactory[CC]
