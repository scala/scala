package strawman
package collection

/**
  * A multimap whose keys are sorted
  * @tparam K the type of keys
  * @tparam V the type of values
  */
trait SortedMultiMap[K, V]
  extends MultiMap[K, V]
    with SortedMultiMapOps[K, V, SortedMultiMap, SortedMultiMap[K, V]] {

  def unsorted: MultiMap[K, V] = this

}

trait SortedMultiMapOps[K, V, +CC[X, Y] <: MultiMap[X, Y], +C <: MultiMap[K, V]]
  extends MultiMapOps[K, V, MultiMap, C]
    with SortedOps[K, C] {

  def multiMapFactory: MapFactory[MultiMap] = MultiMap
  def sortedMultiMapFactory: SortedMapFactory[CC]

  protected[this] def sortedFromIterable[L : Ordering, W](it: Iterable[(L, W)]): CC[L, W]
  protected[this] def sortedFromSets[L : Ordering, W](it: Iterable[(L, Set[W])]): CC[L, W] =
    sortedFromIterable(it.view.flatMap { case (l, ws) => ws.map(w => (l, w)) })

  /** `this` sorted multimap upcasted to an unsorted multimap */
  def unsorted: MultiMap[K, V]

  def sets: SortedMap[K, Set[V]]

  def iteratorFrom(start: K): Iterator[(K, V)] =
    sets.iteratorFrom(start).flatMap { case (k, vs) => vs.view.map(v => (k, v)) }

  def firstKey: K = sets.firstKey

  def lastKey: K = sets.lastKey

  def rangeTo(to: K): C = {
    val i = from(to).iterator()
    if (i.isEmpty) return coll
    val next = i.next()._1
    if (ordering.compare(next, to) == 0)
      if (i.isEmpty) coll
      else until(i.next()._1)
    else
      until(next)
  }

  override def withFilter(p: ((K, V)) => Boolean): SortedMultiMapWithFilter = new SortedMultiMapWithFilter(p)

  class SortedMultiMapWithFilter(p: ((K, V)) => Boolean) extends MultiMapWithFilter(p) {
    def map[L : Ordering, W](f: ((K, V)) => (L, W)): CC[L, W] = sortedFromIterable(View.Map(filtered, f))
    def flatMap[L : Ordering, W](f: ((K, V)) => IterableOnce[(L, W)]): CC[L, W] = sortedFromIterable(View.FlatMap(filtered, f))
    override def withFilter(q: ((K, V)) => Boolean): SortedMultiMapWithFilter = new SortedMultiMapWithFilter(kv => p(kv) && q(kv))
  }

  /**
    * @return a sorted multimap that contains all the entries of `this` sorted multimap,
    *         transformed by the function `f`
    *
    * @param f transformation function
    * @tparam L new type of keys
    * @tparam W new type of values
    */
  def map[L : Ordering, W](f: ((K, V)) => (L, W)): CC[L, W] = sortedFromIterable(View.Map(toIterable, f))

  /**
    * Builds a new sorted multimap by applying a function to all groups of elements
    *
    * @param f  the function to apply
    * @tparam L the type of keys of the returned collection
    * @return   a new collection resulting from applying the given function
    *           `f` to each pair of element and its number of occurrences of this
    *           sorted multiset and collecting the results.
    */
  def mapSets[L : Ordering, W](f: ((K, Set[V])) => (L, Set[W])): CC[L, W] = sortedFromSets(View.Map(sets, f))

  /**
    * @return a sorted multimap that contains all the entries of `this` sorted multimap,
    *         transformed by the function `f` and concatenated
    *
    * @param f transformation function
    * @tparam L new type of keys
    * @tparam W new type of values
    */
  def flatMap[L : Ordering, W](f: ((K, V)) => IterableOnce[(L, W)]): CC[L, W] = sortedFromIterable(View.FlatMap(toIterable, f))

  /**
    * @return a new sorted multimap resulting from applying the given function `f`
    *         to each group of values of this sorted multimap and concatenating
    *         the results
    * @param f the function to apply
    * @tparam L the new type of keys
    * @tparam W the type of values of the returned sorted multimap
    */
  def flatMapSets[L : Ordering, W](f: ((K, Set[V])) => IterableOnce[(L, Set[W])]): CC[L, W] = sortedFromSets(View.FlatMap(sets, f))

  /**
    * @return a sorted multimap that contains all the entries of `this` sorted multimap
    *         after they have been successfully transformed by the
    *         given partial function `pf`
    *
    * @param pf transformation to apply
    * @tparam L new type of keys
    * @tparam W new type of values
    */
  def collect[L : Ordering, W](pf: PartialFunction[(K, V), (L, W)]): CC[L, W] = flatMap(kv =>
    if (pf.isDefinedAt(kv)) View.Single(pf(kv))
    else View.Empty
  )

  /**
    * @return a sorted multimap that contains all the entries of `this` sorted multimap,
    *         after they have been successfully transformed by the given
    *         partial function
    *
    * @param pf the partial function to apply to each set of values
    * @tparam L the new type of keys
    * @tparam W the new type of values
    */
  def collectSets[L : Ordering, W](pf: PartialFunction[(K, Set[V]), (L, Set[W])]): CC[L, W] = flatMapSets(kv =>
    if (pf.isDefinedAt(kv)) View.Single(pf(kv))
    else View.Empty
  )

}

object SortedMultiMap extends SortedMapFactory.Delegate[SortedMultiMap](immutable.SortedMultiMap)