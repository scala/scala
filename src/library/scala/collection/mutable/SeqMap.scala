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
package collection
package mutable

/**
  * A generic trait for ordered mutable maps. Concrete classes have to provide
  * functionality for the abstract methods in `SeqMap`.
  *
  * Note that when checking for equality [[SeqMap]] does not take into account
  * ordering.
  *
  * @tparam K      the type of the keys contained in this linked map.
  * @tparam V      the type of the values associated with the keys in this linked map.
  *
  * @define coll mutable Seq map
  * @define Coll `mutable.SeqMap`
  */

trait SeqMap[K, V] extends Map[K, V]
  with collection.SeqMap[K, V]
  with MapOps[K, V, SeqMap, SeqMap[K, V]]
  with MapFactoryDefaults[K, V, SeqMap, Iterable] {
  override def mapFactory: MapFactory[SeqMap] = SeqMap
}

object SeqMap extends MapFactory.Delegate[SeqMap](LinkedHashMap)
