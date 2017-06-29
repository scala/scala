package strawman
package collection
package immutable

import mutable.{Builder, ImmutableBuilder}

import scala.annotation.tailrec
import scala.{Any, Boolean, Int, NoSuchElementException, SerialVersionUID, Serializable}

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order whey were first inserted.
  *
  * Elements are stored internally in reversed insertion order, which means the newest element is at
  * the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
  * `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
  * makes this collection suitable only for a small number of elements.
  *
  * Instances of `ListSet` represent empty sets; they can be either created by calling the
  * constructor directly, or by applying the function `ListSet.empty`.
  *
  * @tparam A the type of the elements contained in this list set
  *
  * @author Matthias Zenger
  * @version 1.0, 09/07/2003
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(-8417059026623606218L)
sealed class ListSet[A]
  extends Set[A]
    with SetOps[A, ListSet, ListSet[A]]
    with StrictOptimizedIterableOps[A, ListSet[A]]
    with Serializable {

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A): Boolean = false

  def incl(elem: A): ListSet[A] = new Node(elem)
  def excl(elem: A): ListSet[A] = this

  def empty: ListSet[A] = ListSet.empty

  def iterator(): strawman.collection.Iterator[A] = {
    var curr: ListSet[A] = this
    var res: List[A] = Nil
    while (!curr.isEmpty) {
      res = curr.elem :: res
      curr = curr.next
    }
    res.iterator()
  }

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: ListSet[A] = throw new NoSuchElementException("next of empty set")

  def toSet[B >: A]: Set[B] = this.asInstanceOf[ListSet[B]]

  def iterableFactory = ListSet
  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): ListSet[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, ListSet[A]] = ListSet.newBuilder()

  /**
    * Represents an entry in the `ListSet`.
    */
  @SerialVersionUID(-787710309854855049L)
  protected class Node(override protected val elem: A) extends ListSet[A] with Serializable {

    override def size = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A) = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    override def incl(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    override def excl(e: A): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) acc.foldLeft(cur.next)((t, h) => new t.Node(h.elem))
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSet[A] = ListSet.this

    override def last: A = elem

    override def init: ListSet[A] = next
  }
}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  */
object ListSet extends IterableFactory[ListSet] {

  def fromIterable[E](it: strawman.collection.Iterable[E]): ListSet[E] =
    it match {
      case ls: ListSet[E] => ls
      case _ => empty ++ it
    }

  @SerialVersionUID(5010379588739277132L)
  private object EmptyListSet extends ListSet[Any]
  private[collection] def emptyInstance: ListSet[Any] = EmptyListSet

  def empty[A]: ListSet[A] = EmptyListSet.asInstanceOf[ListSet[A]]

  def newBuilder[A](): Builder[A, ListSet[A]] =
    new ImmutableBuilder[A, ListSet[A]](empty) {
      def add(elem: A): this.type = { elems = elems + elem; this }
    }
}

