package strawman.collection

import scala.{Int, Option}

import strawman.collection.mutable.Builder

trait MapView[K, +V]
  extends MapOps[K, V, ({ type l[X, Y] = View[(X, Y)] })#l, View[(K, V)]]
    with View[(K, V)] {

  override def view: MapView[K, V] = this

  protected[this] def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): View[(K2, V2)] = fromIterable(it)

  def mapFactory: MapFactory[({ type l[X, Y] = View[(X, Y)] })#l] =
    new MapFactory[({ type l[X, Y] = View[(X, Y)] })#l] {
      def newBuilder[X, Y](): Builder[(X, Y), View[(X, Y)]] = View.newBuilder[(X, Y)]()
      def empty[X, Y]: View[(X, Y)] = View.empty
      def from[X, Y](it: IterableOnce[(X, Y)]): View[(X, Y)] = View.from(it.asInstanceOf[Iterable[(X, Y)]]) // Waiting for https://github.com/scala/collection-strawman/pull/428
    }

  def empty: View[(K, V)] = View.Empty

}

object MapView {

  type AnyIterableConstr[X, Y] = IterableOps[_, AnyConstr, _]
  type AnyMapOps[K, +V] = MapOps[K, V, AnyIterableConstr, _]

  class Id[K, +V](underlying: AnyMapOps[K, V]) extends MapView[K, V] {
    def get(key: K): Option[V] = underlying.get(key)
    def iterator(): Iterator[(K, V)] = underlying.iterator()
    override def knownSize: Int = underlying.knownSize
  }

  class MapValues[K, +V, +W](underlying: AnyMapOps[K, V], f: V => W) extends MapView[K, W] {
    def iterator(): Iterator[(K, W)] = underlying.iterator().map(kv => (kv._1, f(kv._2)))
    def get(key: K): Option[W] = underlying.get(key).map(f)
    override def knownSize: Int = underlying.knownSize
  }

}