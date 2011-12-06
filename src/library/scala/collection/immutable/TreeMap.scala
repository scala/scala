/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import mutable.Builder
import annotation.bridge

/** $factoryInfo
 *  @define Coll immutable.TreeMap
 *  @define coll immutable tree map
 */
object TreeMap extends ImmutableSortedMapFactory[TreeMap] {
  def empty[A, B](implicit ord: Ordering[A]) = new TreeMap[A, B]()(ord)
  /** $sortedMapCanBuildFromInfo */
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), TreeMap[A, B]] = new SortedMapCanBuildFrom[A, B]
  private def make[A, B](s: Int, t: RedBlack[A]#Tree[B])(implicit ord: Ordering[A]) = new TreeMap[A, B](s, t)(ord)
}

/** This class implements immutable maps using a tree.
 *
 *  @tparam A         the type of the keys contained in this tree map.
 *  @tparam B         the type of the values associated with the keys.
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
class TreeMap[A, +B](override val size: Int, t: RedBlack[A]#Tree[B])(implicit val ordering: Ordering[A])
  extends RedBlack[A]
     with SortedMap[A, B]
     with SortedMapLike[A, B, TreeMap[A, B]]
     with MapLike[A, B, TreeMap[A, B]]
     with Serializable {

  def isSmaller(x: A, y: A) = ordering.lt(x, y)

  override protected[this] def newBuilder : Builder[(A, B), TreeMap[A, B]] =
    TreeMap.newBuilder[A, B]

  def this()(implicit ordering: Ordering[A]) = this(0, null)(ordering)

  protected val tree: RedBlack[A]#Tree[B] = if (size == 0) Empty else t

  override def rangeImpl(from : Option[A], until : Option[A]): TreeMap[A,B] = {
    val ntree = tree.range(from,until)
    new TreeMap[A,B](ntree.count, ntree)
  }

  override def firstKey = t.first
  override def lastKey = t.last
  override def compare(k0: A, k1: A): Int = ordering.compare(k0, k1)

  /** A factory to create empty maps of the same type of keys.
   */
  override def empty: TreeMap[A, B] = TreeMap.empty[A, B](ordering)

  /** A new TreeMap with the entry added is returned,
   *  if key is <em>not</em> in the TreeMap, otherwise
   *  the key is updated with the new entry.
   *
   *  @tparam B1     type of the value of the new binding which is a supertype of `B`
   *  @param key     the key that should be updated
   *  @param value   the value to be associated with `key`
   *  @return        a new $coll with the updated binding
   */
  override def updated [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    val newsize = if (tree.lookup(key).isEmpty) size + 1 else size
    TreeMap.make(newsize, tree.update(key, value))
  }

  /** Add a key/value pair to this map.
   *  @tparam   B1   type of the value of the new binding, a supertype of `B`
   *  @param    kv   the key/value pair
   *  @return        A new $coll with the new binding added to this map
   */
  override def + [B1 >: B] (kv: (A, B1)): TreeMap[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @tparam B1   type of the values of the new bindings, a supertype of `B`
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return      a new $coll with the updated bindings
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): TreeMap[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs     the traversable object.
   */
  override def ++[B1 >: B] (xs: GenTraversableOnce[(A, B1)]): TreeMap[A, B1] =
    ((repr: TreeMap[A, B1]) /: xs.seq) (_ + _)

  @bridge def ++[B1 >: B] (xs: TraversableOnce[(A, B1)]): TreeMap[A, B1] = ++(xs: GenTraversableOnce[(A, B1)])

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   *
   *  @tparam B1    type of the values of the new bindings, a supertype of `B`
   *  @param key    the key to be inserted
   *  @param value  the value to be associated with `key`
   *  @return       a new $coll with the inserted binding, if it wasn't present in the map
   */
  def insert [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    assert(tree.lookup(key).isEmpty)
    TreeMap.make(size + 1, tree.update(key, value))
  }

  def - (key:A): TreeMap[A, B] =
    if (tree.lookup(key).isEmpty) this
    else if (size == 1) empty
    else TreeMap.make(size - 1, tree.delete(key))

  /** Check if this map maps `key` to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return         the value of the mapping, if it exists
   */
  override def get(key: A): Option[B] = tree.lookup(key) match {
    case n: NonEmpty[b] => Some(n.value)
    case _ => None
  }

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[(A, B)] = tree.toStream.iterator

  override def toStream: Stream[(A, B)] = tree.toStream

  override def foreach[U](f : ((A,B)) =>  U) = tree foreach { case (x, y) => f(x, y) }
}




