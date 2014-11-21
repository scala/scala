/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel
package mutable

import scala.collection.generic._
import scala.collection.mutable.Cloneable
import scala.collection.generic.Growable
import scala.collection.generic.Shrinkable

/** A template trait for mutable parallel maps. This trait is to be mixed in
 *  with concrete parallel maps to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *  @define Coll `ParMap`
 *  @define coll parallel map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMapLike[K,
                 V,
                 +Repr <: ParMapLike[K, V, Repr, Sequential] with ParMap[K, V],
                 +Sequential <: scala.collection.mutable.Map[K, V] with scala.collection.mutable.MapLike[K, V, Sequential]]
extends scala.collection.GenMapLike[K, V, Repr]
   with scala.collection.parallel.ParMapLike[K, V, Repr, Sequential]
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
