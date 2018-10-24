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

/** A template for companion objects of `SortedMap` and subclasses thereof.
 *
 *  @since 2.8
 *  @define Coll `SortedMap`
 *  @define coll sorted map
 *  @define factoryInfo
 *    This object provides a set of operations needed to create sorted maps of type `$Coll`.
 *    @author Martin Odersky
 *  @define sortedMapCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for sorted maps
 */
abstract class ImmutableSortedMapFactory[CC[A, B] <: immutable.SortedMap[A, B] with SortedMapLike[A, B, CC[A, B]]] extends SortedMapFactory[CC]
