package strawman
package collection

import scala.{Boolean, Ordering, Unit}

/** A template trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
  *  of trait `Iterable`.
  *
  * @tparam A Element type (e.g. `Int`)
  * @tparam CC Collection type constructor (e.g. `List`)
  */
class WithFilter[+A, +CC[_]](filtered: View[A], factory: IterableFactory[CC]) {

  def map[B](f: A => B): CC[B] = factory.fromIterable(View.Map(filtered, f))

  def flatMap[B](f: A => IterableOnce[B]): CC[B] = factory.fromIterable(View.FlatMap(filtered, f))

  def foreach[U](f: A => U): Unit = filtered.foreach(f)

  def withFilter(p: A => Boolean): WithFilter[A, CC] = new WithFilter(filtered.filter(p), factory)

}

class SortedWithFilter[+A, +CC1[_], +CC2[X] <: Iterable[X]](
  filtered: View[A],
  iterableFactory: IterableFactory[CC1],
  sortedFactory: SortedIterableFactory[CC2]
) extends WithFilter[A, CC1](filtered, iterableFactory) {

  def map[B : Ordering](f: A => B): CC2[B] = sortedFactory.sortedFromIterable(View.Map(filtered, f))

  def flatMap[B : Ordering](f: A => IterableOnce[B]): CC2[B] = sortedFactory.sortedFromIterable(View.FlatMap(filtered, f))

  override def withFilter(p: A => Boolean): SortedWithFilter[A, CC1, CC2] =
    new SortedWithFilter(filtered.filter(p), iterableFactory, sortedFactory)

}

class MapWithFilter[K, +V, +CC1[_], +CC2[X, Y] <: Map[X, Y]](
  filtered: View[(K, V)],
  iterableFactory: IterableFactory[CC1],
  mapFactory: MapFactory[CC2]
) extends WithFilter[(K, V), CC1](filtered, iterableFactory) {

  def map[K2, V2](f: ((K, V)) => (K2, V2)): CC2[K2, V2] = mapFactory.fromIterable(View.Map(filtered, f))

  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC2[K2, V2] = mapFactory.fromIterable(View.FlatMap(filtered, f))

  override def withFilter(p: ((K, V)) => Boolean): MapWithFilter[K, V, CC1, CC2] =
    new MapWithFilter(filtered.filter(p), iterableFactory, mapFactory)

}

class SortedMapWithFilter[K, +V, +CC1[_], +CC2[X, Y] <: Map[X, Y], +CC3[_, _]](
  filtered: View[(K, V)],
  iterableFactory: IterableFactory[CC1],
  mapFactory: MapFactory[CC2],
  sortedMapFactory: SortedMapFactory[CC3]
) extends MapWithFilter[K, V, CC1, CC2](filtered, iterableFactory, mapFactory) {

  def map[K2 : Ordering, V2](f: ((K, V)) => (K2, V2)): CC3[K2, V2] =
    sortedMapFactory.sortedFromIterable(View.Map(filtered, f))

  def flatMap[K2 : Ordering, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC3[K2, V2] =
    sortedMapFactory.sortedFromIterable(View.FlatMap(filtered, f))

  override def withFilter(p: ((K, V)) => Boolean): SortedMapWithFilter[K, V, CC1, CC2, CC3] =
    new SortedMapWithFilter(filtered.filter(p), iterableFactory, mapFactory, sortedMapFactory)

}
