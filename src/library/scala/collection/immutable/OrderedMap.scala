package scala
package collection
package immutable

/** This class implements an immutable map that preserves order using
  * a pair of underlying maps: a tree map used for iteration in
  * insertion or modification order (see below) and a hash map used for
  * efficient lookup.
  *
  * By default insertion order (specified by `OrderedMap.OrderBy.Insertion`)
  * is used, but modification order can be used instead by specifying
  * `OrderedMap.OrderBy.Modification` to the various factory methods.
  * An existing ordered map can be made to use a specific ordering by
  * calling the `orderedBy(orderBy: OrderedMap.OrderBy): OrderedMap[K, V]`
  * method.
  *
  * A key can be manually refreshed (i.e. placed at the end) via the
  * `refresh(key: K): OrderedMap[K, V]` method (regardless of the ordering in
  * use).

  * Internally, an ordinal counter is increased for each insertion/modification
  * and then the current ordinal is used as key in the tree map. After 2^32^
  * insertions/modifications the entire map is copied (thus resetting the ordinal
  * counter).
  *
  *
  *  @tparam K the type of the keys contained in this map.
  *  @tparam V the type of the values associated with the keys in this map.
  *
  * @author Odd Möller
  * @version 2.13
  * @since 2.13
  * @define coll immutable ordered map
  * @define Coll `immutable.OrderedMap`
  */
final class OrderedMap[K, +V] private(
    private[immutable] val ordering: TreeMap[Int, K],
    private[immutable] val mapping: HashMap[K, (Int, V)],
    private[immutable] val ordinal: Int,
    val orderBy: OrderedMap.OrderBy)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, OrderedMap, OrderedMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, OrderedMap[K, V]] {

  import OrderedMap._

  override protected[this] def className: String = "OrderedMap"

  override def mapFactory: MapFactory[OrderedMap] = OrderedMap

  override val size = mapping.size

  override def knownSize: Int = size

  override def isEmpty = size == 0

  def orderedBy(orderBy: OrderBy): OrderedMap[K, V] = {
    if (orderBy == this.orderBy) this
    else new OrderedMap(ordering, mapping, ordinal, orderBy)
  }

  def updated[V1 >: V](key: K, value: V1): OrderedMap[K, V1] = {
    mapping.get(key) match {
      case Some((o, _)) if orderBy == OrderBy.Insertion =>
        new OrderedMap(
          ordering.updated(o, key),
          mapping.updated(key, (o, value)),
          o,
          orderBy)
      case _ if ordinal == Int.MaxValue =>
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        OrderedMap.empty[K, V](orderBy) ++ this + (key → value)
      case Some((o, _)) =>
        val o1 = ordinal + 1
        new OrderedMap(
          ordering.remove(o).updated(o1, key),
          mapping.updated(key, (o1, value)),
          o1,
          orderBy)
      case None ⇒
        val o1 = ordinal + 1
        new OrderedMap(
          ordering.updated(o1, key),
          mapping.updated(key, (o1, value)),
          o1,
          orderBy)
    }
  }

  def remove(key: K): OrderedMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) ⇒
        new OrderedMap(
          ordering.remove(o),
          mapping.remove(key),
          ordinal,
          orderBy)
      case None ⇒
        this
    }
  }

  override def contains(key: K): Boolean = mapping.contains(key)

  def refresh(key: K): OrderedMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) ⇒
        val o1 = ordinal + 1
        new OrderedMap(
          ordering.remove(o).updated(o1, key),
          mapping,
          o1,
          orderBy)
      case None ⇒
        this
    }
  }

  override def head: (K, V) = binding(ordering.head._2)

  override def headOption = ordering.headOption.flatMap {
    case (_, k) ⇒ bindingOption(k)
  }

  override def last: (K, V) = binding(ordering.last._2)

  override def lastOption: Option[(K, V)] = ordering.lastOption.flatMap {
    case (_, k) ⇒ bindingOption(k)
  }

  override def tail: OrderedMap[K, V] = {
    new OrderedMap(ordering.tail, mapping.remove(key(ordering.head)), ordinal, orderBy)
  }

  override def init: OrderedMap[K, V] = {
    new OrderedMap(ordering.init, mapping.remove(key(ordering.last)), ordinal, orderBy)
  }

  def get(key: K): Option[V] = mapping.get(key).map(value)

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): (K, V) = binding(key(iter.next()))
  }

  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): K = key(iter.next())
  }

  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): V = value(binding(key(iter.next())))
  }

  @`inline` private[this] def key(p: (Int, K)) = p._2
  @`inline` private[this] def value(p: (Int, V)) = p._2
  @`inline` private[this] def value(p: (K, V)) = p._2
  @`inline` private[this] def binding(k: K) = mapping(k).copy(_1 = k)
  @`inline` private[this] def bindingOption(k: K) = mapping.get(k).map(_.copy(_1 = k))
}
object OrderedMap extends MapFactory[OrderedMap] {
  sealed trait OrderBy
  final object OrderBy {
    final case object Insertion extends OrderBy
    final case object Modification extends OrderBy
  }

  def empty[K, V]: OrderedMap[K, V] = empty(OrderBy.Insertion)
  def empty[K, V](orderBy: OrderBy): OrderedMap[K, V] = new OrderedMap(TreeMap.empty, HashMap.empty, Int.MinValue, orderBy)

  def from[K, V](it: collection.IterableOnce[(K, V)]): OrderedMap[K, V] =
    it match {
      case vm: OrderedMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: mutable.Builder[(K, V), OrderedMap[K, V]] = newBuilder(OrderBy.Insertion)

  def newBuilder[K, V](orderBy: OrderBy): mutable.Builder[(K, V), OrderedMap[K, V]] =
    new mutable.ImmutableBuilder[(K, V), OrderedMap[K, V]](empty(orderBy)) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }
}
