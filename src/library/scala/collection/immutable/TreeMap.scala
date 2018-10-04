package scala
package collection
package immutable

import scala.collection.immutable.{RedBlackTree => RB}
import scala.collection.mutable.{Builder, ReusableBuilder}


/** This class implements immutable maps using a tree.
  *
  *  @tparam K         the type of the keys contained in this tree map.
  *  @tparam V         the type of the values associated with the keys.
  *  @param ordering   the implicit ordering used to compare objects of type `A`.
  *
  *  @author  Erik Stenman
  *  @author  Matthias Zenger
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
  *  section on `Red-Black Trees` for more information.
  *
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
final class TreeMap[K, +V] private (private val tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with SortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeMap[K, V]]
    with StrictOptimizedMapOps[K, V, Map, TreeMap[K, V]]
    with StrictOptimizedSortedMapOps[K, V, TreeMap, TreeMap[K, V]] {

  def this()(implicit ordering: Ordering[K]) = this(null)(ordering)

  override def sortedMapFactory = TreeMap

  def iterator: Iterator[(K, V)] = RB.iterator(tree)

  def keysIteratorFrom(start: K): Iterator[K] = RB.keysIterator(tree, Some(start))

  def iteratorFrom(start: K): Iterator[(K, V)] = RB.iterator(tree, Some(start))

  override def valuesIteratorFrom(start: K): Iterator[V] = RB.valuesIterator(tree, Some(start))

  def get(key: K): Option[V] = RB.get(tree, key)

  def remove(key: K): TreeMap[K,V] = {
    val t = RB.delete(tree, key)
    if(t eq tree) this else new TreeMap(t)
  }

  def updated[V1 >: V](key: K, value: V1): TreeMap[K, V1] = {
    val t = RB.update(tree, key, value, overwrite = true)
    if(t eq tree) this else new TreeMap(t)
  }

  override def concat[V1 >: V](that: collection.IterableOnce[(K, V1)]): TreeMap[K, V1] = {
    val t = that match {
      case tm: TreeMap[K, V] if ordering == tm.ordering =>
        RB.union(tree, tm.tree)
      case _ =>
        val it = that.iterator
        var t: RB.Tree[K, V1] = tree
        while (it.hasNext) {
          val (k, v) = it.next()
          t = RB.update(t, k, v, overwrite = true)
        }
        if(t eq tree) this else new TreeMap(t)
        t
    }
    if(t eq tree) this else new TreeMap(t)
  }

  override def removeAll(keys: IterableOnce[K]): TreeMap[K, V] = keys match {
    case ts: TreeSet[K] if ordering == ts.ordering =>
      val t = RB.difference(tree, ts.tree)
      if(t eq tree) this else new TreeMap(t)
    case _ => super.removeAll(keys)
  }

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   *
   *  @tparam V1    type of the values of the new bindings, a supertype of `V`
   *  @param key    the key to be inserted
   *  @param value  the value to be associated with `key`
   *  @return       a new $coll with the inserted binding, if it wasn't present in the map
   */
  @deprecated("Use `updated` instead", "2.13.0")
  def insert[V1 >: V](key: K, value: V1): TreeMap[K, V1] = {
    assert(!RB.contains(tree, key))
    updated(key, value)
  }

  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = new TreeMap[K, V](RB.rangeImpl(tree, from, until))

  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  override def range(from: K, until: K): TreeMap[K,V] = new TreeMap[K, V](RB.range(tree, from, until))

  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)

  override def size: Int = RB.count(tree)
  override def knownSize: Int = size

  override def isEmpty = size == 0

  override def firstKey: K = RB.smallest(tree).key

  override def lastKey: K = RB.greatest(tree).key

  override def head: (K, V) = {
    val smallest = RB.smallest(tree)
    (smallest.key, smallest.value)
  }

  override def last: (K, V) = {
    val greatest = RB.greatest(tree)
    (greatest.key, greatest.value)
  }

  override def tail: TreeMap[K, V] = new TreeMap(RB.delete(tree, firstKey))

  override def init: TreeMap[K, V] = new TreeMap(RB.delete(tree, lastKey))

  override def drop(n: Int): TreeMap[K, V] = {
    if (n <= 0) this
    else if (n >= size) empty
    else new TreeMap(RB.drop(tree, n))
  }

  override def take(n: Int): TreeMap[K, V] = {
    if (n <= 0) empty
    else if (n >= size) this
    else new TreeMap(RB.take(tree, n))
  }

  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new TreeMap(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int): TreeMap[K, V] = take(size - math.max(n, 0))

  override def takeRight(n: Int): TreeMap[K, V] = drop(size - math.max(n, 0))

  private[this] def countWhile(p: ((K, V)) => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }

  override def dropWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = drop(countWhile(p))

  override def takeWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = take(countWhile(p))

  override def span(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = splitAt(countWhile(p))

  override protected[this] def className = "TreeMap"
}

/** $factoryInfo
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  */
@SerialVersionUID(3L)
object TreeMap extends SortedMapFactory[TreeMap] {

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap()

  def from[K, V](it: IterableOnce[(K, V)])(implicit ordering: Ordering[K]): TreeMap[K, V] =
    it match {
      case tm: TreeMap[K, V] if ordering == tm.ordering => tm
      case sm: scala.collection.SortedMap[K, V] if ordering == sm.ordering =>
        new TreeMap[K, V](RB.fromOrderedEntries(sm.iterator, sm.size))
      case _ =>
        var t: RB.Tree[K, V] = null
        val i = it.iterator
        while (i.hasNext) {
          val (k, v) = i.next()
          t = RB.update(t, k, v, overwrite = true)
        }
        new TreeMap[K, V](t)
    }

  def newBuilder[K, V](implicit ordering: Ordering[K]): Builder[(K, V), TreeMap[K, V]] = new ReusableBuilder[(K, V), TreeMap[K, V]] {
    private[this] var tree: RB.Tree[K, V] = null
    def addOne(elem: (K, V)): this.type = { tree = RB.update(tree, elem._1, elem._2, overwrite = true); this }
    override def addAll(xs: IterableOnce[(K, V)]): this.type = {
      xs match {
        case tm: TreeMap[K, V] if ordering == tm.ordering =>
          tree = RB.union(tree, tm.tree)
        case _ =>
          val it = xs.iterator
          while (it.hasNext) {
            val (k, v) = it.next()
            tree = RB.update(tree, k, v, overwrite = true)
          }
      }
      this
    }
    def result(): TreeMap[K, V] = if(tree eq null) TreeMap.empty else new TreeMap[K, V](tree)
    def clear(): Unit = { tree = null }
  }
}
