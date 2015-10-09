package scala
package collection
package generic

import scala.language.higherKinds

/**
 * A template for companion objects of `SortedMap` and subclasses thereof.
 *
 * @tparam CC the type of the collection.
 *
 * @author Rui Gonçalves
 * @since 2.12
 * @version 2.12
 *
 * @define Coll `mutable.SortedMap`
 * @define coll mutable sorted map
 * @define factoryInfo
 *         This object provides a set of operations needed to create sorted maps of type `$Coll`.
 * @define sortedMapCanBuildFromInfo
 *         The standard `CanBuildFrom` instance for sorted maps
 */
abstract class MutableSortedMapFactory[CC[A, B] <: mutable.SortedMap[A, B] with SortedMapLike[A, B, CC[A, B]]]
  extends SortedMapFactory[CC]
