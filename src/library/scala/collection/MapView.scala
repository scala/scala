package scala.collection


import scala.collection.mutable.Builder

trait MapView[K, +V]
  extends MapOps[K, V, ({ type l[X, Y] = View[(X, Y)] })#l, View[(K, V)]]
    with View[(K, V)] {

  override def view: MapView[K, V] = this

  /** Filters this map by retaining only keys satisfying a predicate.
    *  @param  p   the predicate used to test keys
    *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
    *          the predicate `p`. The resulting map wraps the original map without copying any elements.
    */
  override def filterKeys(p: K => Boolean): MapView[K, V] = new MapView.FilterKeys(this, p)

  /** Transforms this map by applying a function to every retrieved value.
    *  @param  f   the function used to transform values of this map.
    *  @return a map view which maps every key of this map
    *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
    */
  override def mapValues[W](f: V => W): MapView[K, W] = new MapView.MapValues(this, f)

  def mapFactory: MapFactory[({ type l[X, Y] = View[(X, Y)] })#l] = new MapView.MapViewMapFactory[K, V]

  def empty: View[(K, V)] = View.Empty
}

object MapView {

  /** An `IterableOps` whose collection type and collection type constructor are unknown */
  type SomeIterableConstr[X, Y] = IterableOps[_, AnyConstr, _]
  /** A `MapOps` whose collection type and collection type constructor are (mostly) unknown */
  type SomeMapOps[K, +V] = MapOps[K, V, SomeIterableConstr, _]

  @SerialVersionUID(3L)
  class Id[K, +V](underlying: SomeMapOps[K, V]) extends AbstractMapView[K, V] {
    def get(key: K): Option[V] = underlying.get(key)
    def iterator: Iterator[(K, V)] = underlying.iterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class MapValues[K, +V, +W](underlying: SomeMapOps[K, V], f: V => W) extends AbstractMapView[K, W] {
    def iterator: Iterator[(K, W)] = underlying.iterator.map(kv => (kv._1, f(kv._2)))
    def get(key: K): Option[W] = underlying.get(key).map(f)
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class FilterKeys[K, +V](underlying: SomeMapOps[K, V], p: K => Boolean) extends AbstractMapView[K, V] {
    def iterator: Iterator[(K, V)] = underlying.iterator.filter { case (k, _) => p(k) }
    def get(key: K): Option[V] = if (p(key)) underlying.get(key) else None
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  private class MapViewMapFactory[K, V] extends MapFactory[({ type l[X, Y] = View[(X, Y)] })#l] {
    def newBuilder[X, Y]: Builder[(X, Y), View[(X, Y)]] = View.newBuilder[(X, Y)]
    def empty[X, Y]: View[(X, Y)] = View.empty
    def from[X, Y](it: IterableOnce[(X, Y)]): View[(X, Y)] = View.from(it)
  }
}

/** Explicit instantiation of the `MapView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractMapView[K, +V] extends AbstractView[(K, V)] with MapView[K, V]
