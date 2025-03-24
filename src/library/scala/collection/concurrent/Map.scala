/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection.concurrent

import scala.annotation.tailrec

/** A template trait for mutable maps that allow concurrent access.
  *
  *  $concurrentmapinfo
  *
  *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#concurrent_maps "Scala's Collection Library overview"]]
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

  override def getOrElseUpdate(key: K, @deprecatedName("op", since="2.13.13") defaultValue: => V): V = get(key) match {
    case Some(v) => v
    case None =>
      val v = defaultValue
      putIfAbsent(key, v) match {
        case Some(ov) => ov
        case None => v
      }
  }

  /**
   * Removes the entry for the specified key if it's currently mapped to the
   * specified value. Comparison to the specified value is done using reference
   * equality.
   *
   * Not all map implementations can support removal based on reference
   * equality, and for those implementations, object equality is used instead.
   *
   * $atomicop
   *
   * @param k   key for which the entry should be removed
   * @param v   value expected to be associated with the specified key if
   *            the removal is to take place
   * @return    `true` if the removal took place, `false` otherwise
   */
  // TODO: make part of the API in a future version
  private[collection] def removeRefEq(k: K, v: V): Boolean = remove(k, v)

  /**
   * Replaces the entry for the given key only if it was previously mapped to
   * a given value. Comparison to the specified value is done using reference
   * equality.
   *
   * Not all map implementations can support replacement based on reference
   * equality, and for those implementations, object equality is used instead.
   *
   * $atomicop
   *
   * @param k         key for which the entry should be replaced
   * @param oldValue  value expected to be associated with the specified key
   *                  if replacing is to happen
   * @param newValue  value to be associated with the specified key
   * @return          `true` if the entry was replaced, `false` otherwise
   */
  // TODO: make part of the API in a future version
  private[collection] def replaceRefEq(k: K, oldValue: V, newValue: V): Boolean = replace(k, oldValue, newValue)

  /**
   * Update a mapping for the specified key and its current optionally mapped value
   * (`Some` if there is current mapping, `None` if not).
   *
   * If the remapping function returns `Some(v)`, the mapping is updated with the new value `v`.
   * If the remapping function returns `None`, the mapping is removed (or remains absent if initially absent).
   * If the function itself throws an exception, the exception is rethrown, and the current mapping is left unchanged.
   *
   * If the map is updated by another concurrent access, the remapping function will be retried until successfully updated.
   *
   * @param key the key value
   * @param remappingFunction a function that receives current optionally mapped value and return a new mapping
   * @return the new value associated with the specified key
   */
  override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = updateWithAux(key)(remappingFunction)

  @tailrec
  private def updateWithAux(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    val previousValue = get(key)
    val nextValue = remappingFunction(previousValue)
    previousValue match {
      case Some(prev) => nextValue match {
        case Some(next) => if (replaceRefEq(key, prev, next)) return nextValue
        case _          => if (removeRefEq(key, prev)) return None
      }
      case _ => nextValue match {
        case Some(next) => if (putIfAbsent(key, next).isEmpty) return nextValue
        case _          => return None
      }
    }
    updateWithAux(key)(remappingFunction)
  }

  private[collection] def filterInPlaceImpl(p: (K, V) => Boolean): this.type = {
    val it = iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      if (!p(k, v)) removeRefEq(k, v)
    }
    this
  }

  private[collection] def mapValuesInPlaceImpl(f: (K, V) => V): this.type = {
    val it = iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      replaceRefEq(k, v, f(k, v))
    }
    this
  }
}
