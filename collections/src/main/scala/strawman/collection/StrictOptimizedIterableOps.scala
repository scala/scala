package strawman
package collection

import scala.{Any, Boolean, Int, PartialFunction}
import scala.Predef.<:<
import scala.annotation.unchecked.uncheckedVariance

/**
  * Trait that overrides operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam C  Collection type
  */
trait StrictOptimizedIterableOps[+A, +CC[_], +C]
  extends Any
    with IterableOps[A, CC, C] {

  // Optimized, push-based version of `partition`
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder()
    iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  override def span(p: A => Boolean): (C, C) = {
    val first = newSpecificBuilder()
    val second = newSpecificBuilder()
    val it = iterator()
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

  override def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val first = iterableFactory.newBuilder[A1]()
    val second = iterableFactory.newBuilder[A2]()
    foreach { a =>
      val (a1, a2) = asPair(a)
      first += a1
      second += a2
    }
    (first.result(), second.result())
  }

  // The implementations of the following operations are not fundamentally different from
  // the view-based implementations, but they turn out to be slightly faster because
  // a couple of indirection levels are removed

  override def map[B](f: A => B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  override def flatMap[B](f: A => IterableOnce[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  override def collect[B](pf: PartialFunction[A, B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      val elem = it.next()
      if (pf.isDefinedAt(elem)) {
        b += pf.apply(elem)
      }
    }
    b.result()
  }

  override def flatten[B](implicit toIterableOnce: A => IterableOnce[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      b ++= toIterableOnce(it.next())
    }
    b.result()
  }

  override def zip[B](that: Iterable[B]): CC[(A @uncheckedVariance, B)] = {
    val b = iterableFactory.newBuilder[(A, B)]()
    val it1 = iterator()
    val it2 = that.iterator()
    while (it1.hasNext && it2.hasNext) {
      b += ((it1.next(), it2.next()))
    }
    b.result()
  }

  override def zipWithIndex: CC[(A @uncheckedVariance, Int)] = {
    val b = iterableFactory.newBuilder[(A, Int)]()
    var i = 0
    val it = iterator()
    while (it.hasNext) {
      b += ((it.next(), i))
      i += 1
    }
    b.result()
  }

  override def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    b.sizeHint(toIterable, delta = 0)
    var acc = z
    b += acc
    val it = iterator()
    while (it.hasNext) {
      acc = op(acc, it.next())
      b += acc
    }
    b.result()
  }

  override def filter(pred: A => Boolean): C = filterImpl(pred, isFlipped = false)

  override def filterNot(pred: A => Boolean): C = filterImpl(pred, isFlipped = true)

  protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): C = {
    val b = newSpecificBuilder()
    val it = iterator()
    while (it.hasNext) {
      val elem = it.next()
      if (pred(elem) != isFlipped) {
        b += elem
      }
    }
    b.result()
  }

}
