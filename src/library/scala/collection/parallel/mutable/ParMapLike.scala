package scala.collection.parallel
package mutable



import collection.generic._
import collection.mutable.Builder



trait ParMapLike[K,
                 V,
                 +Repr <: ParMapLike[K, V, Repr, Sequential] with ParMap[K, V],
                 +Sequential <: collection.mutable.Map[K, V] with collection.mutable.MapLike[K, V, Sequential]]
extends collection.parallel.ParMapLike[K, V, Repr, Sequential]
   with collection.mutable.MapLike[K, V, Repr]

