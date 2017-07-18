package strawman
package collection
package immutable

import strawman.collection.immutable.{RedBlackTree => RB}
import strawman.collection.mutable.{Builder, ImmutableBuilder}

import scala.{Boolean, Int, math, Option, Ordering, SerialVersionUID, Serializable, Some, Unit}

/** This class implements immutable maps using a tree.
  *
  *  @tparam K         the type of the keys contained in this tree map.
  *  @tparam V         the type of the values associated with the keys.
  *  @param ordering   the implicit ordering used to compare objects of type `A`.
  *
  *  @author  Erik Stenman
  *  @author  Matthias Zenger
  *  @version 1.1, 03/05/2004
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#redblack_trees "Scala's Collection Library overview"]]
  *  section on `Red-Black Trees` for more information.
  *
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(1234)
final class TreeMap[K, +V] private (tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends SortedMap[K, V]
    with SortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeMap[K, V]]
    with Serializable {

  def this()(implicit ordering: Ordering[K]) = this(null)(ordering)

  def iterableFactory = List
  def mapFactory = Map
  def sortedMapFactory = TreeMap

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): TreeMap[K, V] =
    TreeMap.sortedFromIterable(coll)

  protected[this] def sortedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): TreeMap[K2, V2] =
    TreeMap.sortedFromIterable(it)

  protected[this] def newSpecificBuilder(): Builder[(K, V), TreeMap[K, V]] = TreeMap.newBuilder()

  def iterator(): collection.Iterator[(K, V)] = RB.iterator(tree)

  def keysIteratorFrom(start: K): collection.Iterator[K] = RB.keysIterator(tree, Some(start))

  def get(key: K): Option[V] = RB.get(tree, key)

  def remove(key: K): TreeMap[K,V] =
    if (!RB.contains(tree, key)) this
    else new TreeMap(RB.delete(tree, key))

  def updated[V1 >: V](key: K, value: V1): TreeMap[K, V1] = new TreeMap(RB.update(tree, key, value, overwrite = true))

  def empty: TreeMap[K, V] = TreeMap.empty[K, V](ordering)

  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = new TreeMap[K, V](RB.rangeImpl(tree, from, until))

  override def range(from: K, until: K): TreeMap[K,V] = new TreeMap[K, V](RB.range(tree, from, until))

  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)

  override def size: Int = RB.count(tree)

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
    val it = iterator()
    while (it.hasNext && p(it.next())) result += 1
    result
  }

  override def dropWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = drop(countWhile(p))

  override def takeWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = take(countWhile(p))

  override def span(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = splitAt(countWhile(p))

}

/** $factoryInfo
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  */
object TreeMap extends SortedMapFactory[TreeMap] {

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap()

  def sortedFromIterable[K : Ordering, V](it: collection.Iterable[(K, V)]): TreeMap[K, V] =
    it match {
      case tm: TreeMap[K, V] => tm
      case _ => empty[K, V] ++ it
    }

  def newBuilder[K : Ordering, V](): Builder[(K, V), TreeMap[K, V]] =
    new ImmutableBuilder[(K, V), TreeMap[K, V]](empty) {
      def add(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

}
