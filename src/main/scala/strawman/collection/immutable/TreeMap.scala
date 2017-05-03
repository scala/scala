package strawman
package collection
package immutable

import strawman.collection.OrderedMapFactory
import strawman.collection.immutable.{RedBlackTree => RB}
import strawman.collection.mutable.Builder

import scala.{Int, Option, Ordering, SerialVersionUID, Serializable, Some, Unit}

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
    with Serializable {

  def this()(implicit ordering: Ordering[K]) = this(null)(ordering)

  protected[this] def fromIterable[E](it: collection.Iterable[E]): Iterable[E] = List.fromIterable(it)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): TreeMap[K, V] =
    TreeMap.fromIterable(coll)

  protected[this] def orderedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): TreeMap[K2, V2] =
    TreeMap.fromIterable(it)

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

}

/** $factoryInfo
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  */
object TreeMap extends OrderedMapFactory[TreeMap] {

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap()

  def fromIterable[K : Ordering, V](it: collection.Iterable[(K, V)]): TreeMap[K, V] =
    it match {
      case tm: TreeMap[K, V] => tm
      case _ => empty[K, V] ++ it
    }

}
