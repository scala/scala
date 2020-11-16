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

import generic._
import scala.runtime.AbstractFunction1

/** A trait for all traversable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenMap[K, +V]
extends GenMapLike[K, V, GenMap[K, V]]
   with GenIterable[(K, V)]
{
  def seq: Map[K, V]

  def updated [V1 >: V](key: K, value: V1): GenMap[K, V1]
}

object GenMap extends GenMapFactory[GenMap] {
  def empty[K, V]: immutable.Map[K, V] = immutable.Map.empty

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), GenMap[K, V]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (K, V), GenMap[K, V]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Nothing, Nothing]

  private[collection] def mapEquals[K1, V, K2](thisMap: GenMapLike[K1, V, _], thatMap: GenMap[K2, _]): Boolean = {
    (thisMap eq thatMap) ||
      (thatMap canEqual thisMap) &&
        (thisMap.size == thatMap.size) && {
        try {
          val checker = new AbstractFunction1[(K1, V),Boolean] with Function0[V]{
            override def apply(kv: (K1,V)): Boolean = {
              // Note: uncurry optimizes this to `get.getOrElse(..., this: Function0)`;  there is no extra lambda allocated.
              val v2 = thatMap.getOrElse(kv._1.asInstanceOf[K2], this.apply())
              // A mis-behaving user-defined equals method might not expect the sentinel value, and we should try to limit
              // the chance of it escaping. Its also probably quicker to avoid the virtual call to equals.
              (v2.asInstanceOf[AnyRef] ne this) && v2 == kv._2
            }
            override def apply(): V = this.asInstanceOf[V]
          }
          thisMap forall checker
        } catch {
          case ex: ClassCastException => false
        }}
  }
}
