package strawman
package collection

import scala.{Ordering, PartialFunction}

import scala.annotation.unchecked.uncheckedVariance

trait StrictOptimizedSortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SortedSetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, Set, C] {

  override def map[B : Ordering](f: A => B): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  override def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  override def zip[B](that: Iterable[B])(implicit ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = { // sound bcs of VarianceNot
    val b = sortedIterableFactory.newBuilder[(A, B)]()
    val it1 = iterator()
    val it2 = that.iterator()
    while (it1.hasNext && it2.hasNext) {
      b += ((it1.next(), it2.next()))
    }
    b.result()
  }

  override def collect[B : Ordering](pf: PartialFunction[A, B]): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]()
    val it = iterator()
    while (it.hasNext) {
      val elem = it.next()
      if (pf.isDefinedAt(elem)) {
        b += pf.apply(elem)
      }
    }
    b.result()
  }

}
