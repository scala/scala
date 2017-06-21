package strawman
package collection

import scala.{Any, Boolean}

/**
  * Trait that overrides operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam C  Collection type
  */
trait StrictOptimizedIterableOps[+A, +C]
  extends Any
    with IterableOps[A, AnyConstr, C] {

  /** Optimized, push-based version of `partition`. */
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder()
    coll.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  // one might also override other transforms here to avoid generating
  // iterators if it helps efficiency.

}
