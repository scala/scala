package scala.tools.nsc.util

import collection.mutable.HashMap
import collection.immutable

/** A hashmap with set-valued values, and an empty set as default value
 */
class MultiHashMap[K, V] extends HashMap[K, immutable.Set[V]] {
  override def default(key: K): immutable.Set[V] = Set()
}
