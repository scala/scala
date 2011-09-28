/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.parallel
package mutable



import collection.generic._
import collection.mutable.Builder
import collection.mutable.Cloneable
import collection.generic.Growable
import collection.generic.Shrinkable



/** A template trait for mutable parallel maps. This trait is to be mixed in
 *  with concrete parallel maps to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMapLike[K,
                 V,
                 +Repr <: ParMapLike[K, V, Repr, Sequential] with ParMap[K, V],
                 +Sequential <: collection.mutable.Map[K, V] with collection.mutable.MapLike[K, V, Sequential]]
extends collection.GenMapLike[K, V, Repr]
   with collection.parallel.ParMapLike[K, V, Repr, Sequential]
   with Growable[(K, V)]
   with Shrinkable[K]
   with Cloneable[Repr]
{
  // note: should not override toMap

  def put(key: K, value: V): Option[V]

  def +=(kv: (K, V)): this.type

  def -=(key: K): this.type

  def +[U >: V](kv: (K, U)) = this.clone().asInstanceOf[ParMap[K, U]] += kv

  def -(key: K) = this.clone() -= key

  def clear(): Unit

}

