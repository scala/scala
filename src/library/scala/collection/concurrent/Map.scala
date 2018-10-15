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
package collection.concurrent


/** A template trait for mutable maps that allow concurrent access.
  *
  *  $concurrentmapinfo
  *
  *  @since 2.8
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#concurrent_maps "Scala's Collection Library overview"]]
  *  section on `Concurrent Maps` for more information.
  *
  *  @tparam K  the key type of the map
  *  @tparam V  the value type of the map
  *
  *  @define Coll `concurrent.Map`
  *  @define coll concurrent map
  *  @define concurrentmapinfo
  *  This is a base trait for all Scala concurrent map implementations. It
  *  provides all of the methods a `Map` does, with the difference that all the
  *  changes are atomic. It also describes methods specific to concurrent maps.
  *
  *  '''Note''': The concurrent maps do not accept `'''null'''` for keys or values.
  *
  *  @define atomicop
  *  This is an atomic operation.
  */
trait Map[K, V] extends scala.collection.mutable.Map[K, V] {

  /**
    * Associates the given key with a given value, unless the key was already
    * associated with some other value.
    *
    * $atomicop
    *
    * @param k   key with which the specified value is to be associated with
    * @param v   value to be associated with the specified key
    * @return    `Some(oldvalue)` if there was a value `oldvalue` previously
    *            associated with the specified key, or `None` if there was no
    *            mapping for the specified key
    */
  def putIfAbsent(k: K, v: V): Option[V]

  /**
    * Removes the entry for the specified key if it's currently mapped to the
    * specified value.
    *
    * $atomicop
    *
    * @param k   key for which the entry should be removed
    * @param v   value expected to be associated with the specified key if
    *            the removal is to take place
    * @return    `true` if the removal took place, `false` otherwise
    */
  def remove(k: K, v: V): Boolean

  /**
    * Replaces the entry for the given key only if it was previously mapped to
    * a given value.
    *
    * $atomicop
    *
    * @param k         key for which the entry should be replaced
    * @param oldvalue  value expected to be associated with the specified key
    *                  if replacing is to happen
    * @param newvalue  value to be associated with the specified key
    * @return          `true` if the entry was replaced, `false` otherwise
    */
  def replace(k: K, oldvalue: V, newvalue: V): Boolean

  /**
    * Replaces the entry for the given key only if it was previously mapped
    * to some value.
    *
    * $atomicop
    *
    * @param k   key for which the entry should be replaced
    * @param v   value to be associated with the specified key
    * @return    `Some(v)` if the given key was previously mapped to some value `v`, or `None` otherwise
    */
  def replace(k: K, v: V): Option[V]

  override def getOrElseUpdate(key: K, op: =>V): V = get(key) match {
    case Some(v) => v
    case None =>
      val v = op
      putIfAbsent(key, v) match {
        case Some(nv) => nv
        case None => v
      }
  }

}
