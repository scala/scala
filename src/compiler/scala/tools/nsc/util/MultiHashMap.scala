package scala.tools.nsc.util

import scala.collection.{ mutable, immutable }

/** A hashmap with set-valued values, and an empty set as default value
 */
class MultiHashMap[K, V] extends mutable.HashMap[K, immutable.Set[V]] {
  override def default(key: K): immutable.Set[V] = Set()
}
