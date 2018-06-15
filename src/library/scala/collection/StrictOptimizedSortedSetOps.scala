package scala
package collection


import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

trait StrictOptimizedSortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SortedSetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, Set, C] {

  override def map[B](f: A => B)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]
    val it = iterator
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  override def flatMap[B](f: A => IterableOnce[B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]
    val it = iterator
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  override def zip[B](that: Iterable[B])(implicit @implicitNotFound(SortedSetOps.zipOrdMsg) ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = { // sound bcs of VarianceNot
    val b = sortedIterableFactory.newBuilder[(A, B)]
    val it1 = iterator
    val it2 = that.iterator
    while (it1.hasNext && it2.hasNext) {
      b += ((it1.next(), it2.next()))
    }
    b.result()
  }

  override def collect[B](pf: PartialFunction[A, B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] = {
    val b = sortedIterableFactory.newBuilder[B]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      if (pf.isDefinedAt(elem)) {
        b += pf.apply(elem)
      }
    }
    b.result()
  }

}
