package scala
package collection
package immutable

import mutable.{Builder, ImmutableBuilder}
import immutable.{RedBlackTree => RB}


/** This class implements immutable sorted sets using a tree.
  *
  *  @tparam A         the type of the elements contained in this tree set
  *  @param ordering   the implicit ordering used to compare objects of type `A`
  *
  *  @author  Martin Odersky
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
  *  section on `Red-Black Trees` for more information.
  *
  *  @define Coll `immutable.TreeSet`
  *  @define coll immutable tree set
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
final class TreeSet[A] private (tree: RB.Tree[A, Unit])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with StrictOptimizedIterableOps[A, Set, TreeSet[A]]
    with StrictOptimizedSortedSetOps[A, TreeSet, TreeSet[A]] {

  if (ordering eq null) throw new NullPointerException("ordering must not be null")

  def this()(implicit ordering: Ordering[A]) = this(null)(ordering)

  override def sortedIterableFactory = TreeSet

  private def newSet(t: RB.Tree[A, Unit]) = new TreeSet[A](t)

  override def size: Int = RB.count(tree)

  override def isEmpty = size == 0

  override def head: A = RB.smallest(tree).key

  override def last: A = RB.greatest(tree).key

  override def tail: TreeSet[A] = new TreeSet(RB.delete(tree, firstKey))

  override def init: TreeSet[A] = new TreeSet(RB.delete(tree, lastKey))

  override def drop(n: Int): TreeSet[A] = {
    if (n <= 0) this
    else if (n >= size) empty
    else newSet(RB.drop(tree, n))
  }

  override def take(n: Int): TreeSet[A] = {
    if (n <= 0) empty
    else if (n >= size) this
    else newSet(RB.take(tree, n))
  }

  override def slice(from: Int, until: Int): TreeSet[A] = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else newSet(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int): TreeSet[A] = take(size - math.max(n, 0))

  override def takeRight(n: Int): TreeSet[A] = drop(size - math.max(n, 0))

  private[this] def countWhile(p: A => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }
  override def dropWhile(p: A => Boolean): TreeSet[A] = drop(countWhile(p))

  override def takeWhile(p: A => Boolean): TreeSet[A] = take(countWhile(p))

  override def span(p: A => Boolean): (TreeSet[A], TreeSet[A]) = splitAt(countWhile(p))

  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)

  override def minAfter(key: A): Option[A] = {
    val v = RB.minAfter(tree, key)
    if (v eq null) Option.empty else Some(v.key)
  }

  override def maxBefore(key: A): Option[A] = {
    val v = RB.maxBefore(tree, key)
    if (v eq null) Option.empty else Some(v.key)
  }

  def iterator: Iterator[A] = RB.keysIterator(tree)

  def iteratorFrom(start: A): Iterator[A] = RB.keysIterator(tree, Some(start))

  /** Checks if this set contains element `elem`.
    *
    *  @param  elem    the element to check for membership.
    *  @return true, iff `elem` is contained in this set.
    */
  def contains(elem: A): Boolean = RB.contains(tree, elem)

  override def range(from: A, until: A): TreeSet[A] = newSet(RB.range(tree, from, until))

  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = newSet(RB.rangeImpl(tree, from, until))

  /** Creates a new `TreeSet` with the entry added.
    *
    *  @param elem    a new element to add.
    *  @return        a new $coll containing `elem` and all the elements of this $coll.
    */
  def incl(elem: A): TreeSet[A] = newSet(RB.update(tree, elem, (), overwrite = false))

  /** Creates a new `TreeSet` with the entry removed.
    *
    *  @param elem    a new element to add.
    *  @return        a new $coll containing all the elements of this $coll except `elem`.
    */
  def excl(elem: A): TreeSet[A] =
    if (!RB.contains(tree, elem)) this
    else newSet(RB.delete(tree, elem))

  override protected[this] def className = "TreeSet"
}

/**
  * $factoryInfo
  *
  *  @define Coll `immutable.TreeSet`
  *  @define coll immutable tree set
  */
@SerialVersionUID(3L)
object TreeSet extends SortedIterableFactory[TreeSet] {

  def empty[A: Ordering]: TreeSet[A] = new TreeSet[A]

  def from[E: Ordering](it: scala.collection.IterableOnce[E]): TreeSet[E] =
    it match {
      case ts: TreeSet[E] => ts
      case _ => (newBuilder[E] ++= it).result()
    }

  def newBuilder[A : Ordering]: Builder[A, TreeSet[A]] =
    new ImmutableBuilder[A, TreeSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }

}
