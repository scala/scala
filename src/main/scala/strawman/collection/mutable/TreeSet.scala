package strawman
package collection.mutable

import collection.SortedIterableFactory
import collection.mutable.{RedBlackTree => RB}

import scala.{Boolean, Int, None, Null, NullPointerException, Option, Ordering, Serializable, SerialVersionUID, Some, Unit}
import java.lang.String

/**
  * A mutable sorted set implemented using a mutable red-black tree as underlying data structure.
  *
  * @param ordering the implicit ordering used to compare objects of type `A`.
  * @tparam A the type of the keys contained in this tree set.
  *
  * @author Rui Gonçalves
  * @version 2.12
  * @since 2.10
  *
  * @define Coll mutable.TreeSet
  * @define coll mutable tree set
  */
// Original API designed in part by Lucien Pereira
@SerialVersionUID(-3642111301929493640L)
sealed class TreeSet[A] private (tree: RB.Tree[A, Null])(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with Serializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  /**
    * Creates an empty `TreeSet`.
    * @param ord the implicit ordering used to compare objects of type `A`.
    * @return an empty `TreeSet`.
    */
  def this()(implicit ord: Ordering[A]) = this(RB.Tree.empty)(ord)

  def iterator(): collection.Iterator[A] = RB.keysIterator(tree)

  protected[this] def sortedFromIterable[B : Ordering](it: collection.Iterable[B]): TreeSet[B] = TreeSet.sortedFromIterable(it)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): TreeSet[A] = TreeSet.sortedFromIterable(coll)

  def iterableFactory = Set

  def keysIteratorFrom(start: A): collection.Iterator[A] = RB.keysIterator(tree, Some(start))

  def empty: TreeSet[A] = TreeSet.empty

  def add(elem: A): this.type = {
    RB.insert(tree, elem, null)
    this
  }

  def subtract(elem: A): this.type = {
    RB.delete(tree, elem)
    this
  }

  def clear(): Unit = RB.clear(tree)

  def contains(elem: A): Boolean = RB.contains(tree, elem)

  def get(elem: A): Option[A] = RB.getKey(tree, elem)

  def unconstrained: collection.Set[A] = this

  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSetView(from, until)

  override def className: String = "TreeSet"

  override def size: Int = RB.size(tree)

  override def isEmpty: Boolean = RB.isEmpty(tree)

  override def head: A = RB.minKey(tree).get

  override def last: A = RB.maxKey(tree).get

  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)


  /**
    * A ranged projection of a [[TreeSet]]. Mutations on this set affect the original set and vice versa.
    *
    * Only keys between this projection's key range will ever appear as elements of this set, independently of whether
    * the elements are added through the original set or through this view. That means that if one inserts an element in
    * a view whose key is outside the view's bounds, calls to `contains` will _not_ consider the newly added element.
    * Mutations are always reflected in the original set, though.
    *
    * @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
    *             bound.
    * @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
    *              bound.
    */
  @SerialVersionUID(7087824939194006086L)
  private[this] final class TreeSetView(from: Option[A], until: Option[A]) extends TreeSet[A](tree) {

    /**
      * Given a possible new lower bound, chooses and returns the most constraining one (the maximum).
      */
    private[this] def pickLowerBound(newFrom: Option[A]): Option[A] = (from, newFrom) match {
      case (Some(fr), Some(newFr)) => Some(ordering.max(fr, newFr))
      case (None, _) => newFrom
      case _ => from
    }

    /**
      * Given a possible new upper bound, chooses and returns the most constraining one (the minimum).
      */
    private[this] def pickUpperBound(newUntil: Option[A]): Option[A] = (until, newUntil) match {
      case (Some(unt), Some(newUnt)) => Some(ordering.min(unt, newUnt))
      case (None, _) => newUntil
      case _ => until
    }

    /**
      * Returns true if the argument is inside the view bounds (between `from` and `until`).
      */
    private[this] def isInsideViewBounds(key: A): Boolean = {
      val afterFrom = from.isEmpty || ordering.compare(from.get, key) <= 0
      val beforeUntil = until.isEmpty || ordering.compare(key, until.get) < 0
      afterFrom && beforeUntil
    }

    override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] =
      new TreeSetView(pickLowerBound(from), pickUpperBound(until))

    override def contains(key: A) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def iterator = RB.keysIterator(tree, from, until)
    override def keysIteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)
//    override def iteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)

    override def size = iterator.length
    override def isEmpty = !iterator.hasNext

    override def head = headOption.get
    /*override*/ def headOption = {
      val elem = if (from.isDefined) RB.minKeyAfter(tree, from.get) else RB.minKey(tree)
      (elem, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e, unt) >= 0 => None
        case _ => elem
      }
    }

    override def last = lastOption.get
    /*override*/ def lastOption = {
      val elem = if (until.isDefined) RB.maxKeyBefore(tree, until.get) else RB.maxKey(tree)
      (elem, from) match {
        case (Some(e), Some(fr)) if ordering.compare(e, fr) < 0 => None
        case _ => elem
      }
    }

    // Using the iterator should be efficient enough; if performance is deemed a problem later, a specialized
    // `foreachKey(f, from, until)` method can be created in `RedBlackTree`. See
    // https://github.com/scala/scala/pull/4608#discussion_r34307985 for a discussion about this.
    override def foreach[U](f: A => U): Unit = iterator.foreach(f)

    override def clone() = super.clone().rangeImpl(from, until)

  }

}

/**
  * @define Coll `mutable.TreeSet`
  * @define coll mutable tree set
  * @factoryInfo
  *   Companion object of TreeSet providing factory related utilities.
  *
  * @author Lucien Pereira
  *
  */
object TreeSet extends SortedIterableFactory[TreeSet] {

  def empty[A : Ordering]: TreeSet[A] = new TreeSet[A]()

  def sortedFromIterable[E : Ordering](it: collection.Iterable[E]): TreeSet[E] = Growable.fromIterable(empty[E], it)

}
