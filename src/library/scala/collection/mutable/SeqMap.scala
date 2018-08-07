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
  * @author Matthew de Detrich
  * @version 2.13
  * @since 2.13
  * @define coll mutable Seq map
  * @define Coll `mutable.SeqMap`
  */

trait SeqMap[K, V] extends AbstractMap[K, V]
  with MapOps[K, V, SeqMap, SeqMap[K, V]]

object SeqMap extends MapFactory.Delegate[SeqMap](LinkedHashMap)
