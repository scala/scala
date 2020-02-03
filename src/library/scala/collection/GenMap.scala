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

}
