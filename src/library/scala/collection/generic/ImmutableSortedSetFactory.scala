/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
 *  @define sortedSetCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for sorted sets
 */
abstract class ImmutableSortedSetFactory[CC[A] <: immutable.SortedSet[A] with SortedSetLike[A, CC[A]]] extends SortedSetFactory[CC]
