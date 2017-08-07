package strawman
package collection

import scala.{Any, Boolean}
import scala.Predef.<:<

/**
  * Trait that overrides operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam C  Collection type
  */
trait StrictOptimizedIterableOps[+A, +CC[_], +C]
  extends Any
    with IterableOps[A, CC, C] {

  /** Optimized, push-based version of `partition`. */
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder()
    coll.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  override def span(p: A => Boolean): (C, C) = {
    val first = newSpecificBuilder()
    val second = newSpecificBuilder()
    val it = coll.iterator()
    var inFirst = true
    while (it.hasNext && inFirst) {
      val a = it.next()
      if (p(a)) {
        first += a
      } else {
        second += a
        inFirst = false
      }
    }
    while (it.hasNext) {
      second += it.next()
    }
    (first.result(), second.result())
  }

  override def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (CC[A1], CC[A2]) = {
    val first = iterableFactory.newBuilder[A1]()
    val second = iterableFactory.newBuilder[A2]()
    coll.foreach { a =>
      val (a1, a2) = asPair(a)
      first += a1
      second += a2
    }
    (first.result(), second.result())
  }

}
