package strawman
package collection
package immutable

import mutable.{Builder, ImmutableBuilder}
import immutable.{RedBlackTree => RB}

import scala.{Boolean, Int, NullPointerException, Option, Ordering, Some, Unit}

/** This class implements immutable sorted sets using a tree.
  *
  *  @tparam A         the type of the elements contained in this tree set
  *  @param ordering   the implicit ordering used to compare objects of type `A`
  *
  *  @author  Martin Odersky
  *  @version 2.0, 02/01/2007
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#redblack_trees "Scala's Collection Library overview"]]
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
  extends SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with StrictOptimizedIterableOps[A, TreeSet[A]] {

  if (ordering eq null) throw new NullPointerException("ordering must not be null")

  def this()(implicit ordering: Ordering[A]) = this(null)(ordering)

  def iterableFactory = Set

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): TreeSet[A] =
    TreeSet.sortedFromIterable(coll)

  protected[this] def sortedFromIterable[B : Ordering](coll: strawman.collection.Iterable[B]): TreeSet[B] =
    TreeSet.sortedFromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, TreeSet[A]] = TreeSet.newBuilder()

  private def newSet(t: RB.Tree[A, Unit]) = new TreeSet[A](t)

  override def size: Int = RB.count(tree)

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

  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)

  def iterator(): Iterator[A] = RB.keysIterator(tree)

  def keysIteratorFrom(start: A): Iterator[A] = RB.keysIterator(tree, Some(start))

  def unordered: Set[A] = this

  /** Checks if this set contains element `elem`.
    *
    *  @param  elem    the element to check for membership.
    *  @return true, iff `elem` is contained in this set.
    */
  def contains(elem: A): Boolean = RB.contains(tree, elem)

  /** A factory to create empty sets of the same type of keys.
    */
  def empty: TreeSet[A] = TreeSet.empty

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
}

object TreeSet extends SortedIterableFactoryWithBuilder[TreeSet] {

  def empty[A: Ordering]: TreeSet[A] = new TreeSet[A]

  def sortedFromIterable[E: Ordering](it: strawman.collection.Iterable[E]): TreeSet[E] =
    it match {
      case ts: TreeSet[E] => ts
      case _ => empty[E] ++ it
    }

  def newBuilder[A : Ordering](): Builder[A, TreeSet[A]] =
    new ImmutableBuilder[A, TreeSet[A]](empty) {
      def add(elem: A): this.type = { elems = elems + elem; this }
    }

}
