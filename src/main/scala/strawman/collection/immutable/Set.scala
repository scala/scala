package strawman
package collection
package immutable

import strawman.collection.mutable.Builder

import scala.{Any, Boolean, Int, deprecatedName, `inline`, None, Option, Serializable, SerialVersionUID, Some, Unit}

/** Base trait for immutable set collections */
trait Set[A] extends Iterable[A] with collection.Set[A] with SetOps[A, Set, Set[A]]

/** Base trait for immutable set operations */
trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends collection.SetOps[A, CC, C] {

  /** Creates a new set with an additional element, unless the element is
    *  already present.
    *
    *  @param elem the element to be added
    *  @return a new set that contains all elements of this set and that also
    *          contains `elem`.
    */
  def incl(elem: A): C

  /** Alias for `incl` */
  @`inline` final def + (elem: A): C = incl(elem)

  /** Creates a new set with a given element removed from this set.
    *
    *  @param elem the element to be removed
    *  @return a new set that contains all elements of this set but that does not
    *          contain `elem`.
    */
  def excl(elem: A): C

  /** Alias for `excl` */
  @`inline` final def - (elem: A): C = excl(elem)

  override def concat(that: collection.Iterable[A]): C = {
    var result: C = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

  def diff(that: collection.Set[A]): C =
    toIterable.foldLeft(empty)((result, elem) => if (that contains elem) result else result + elem)

}

object Set extends IterableFactory[Set] {

  def empty[A]: Set[A] = EmptySet.asInstanceOf[Set[A]]

  def fromIterable[E](it: collection.Iterable[E]): Set[E] =
    it match {
      // We want `SortedSet` (and subclasses, such as `BitSet`) to
      // rebuild themselves to avoid element type widening issues
      case _: SortedSet[E] => empty ++ it
      case s: Set[E]       => s
      case _               => empty ++ it
    }

  def newBuilder[A](): Builder[A, Set[A]] = HashSet.newBuilder()

  // Reusable implementations for the small sets defined below
  trait SmallSet[A] extends Set[A] {
    def iterableFactory: IterableFactory[Set] = Set
    def empty: Set[A] = iterableFactory.empty
    protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): Set[A] = iterableFactory.fromIterable(coll)
    protected[this] def newSpecificBuilder(): Builder[A, Set[A]] = iterableFactory.newBuilder()
  }

  /** An optimized representation for immutable empty sets */
  @SerialVersionUID(-2443710944435909512L)
  private object EmptySet extends Set[Any] with SmallSet[Any] with Serializable {
    override def size: Int = 0
    def contains(elem: Any): Boolean = false
    def incl(elem: Any): Set[Any] = new Set1(elem)
    def excl(elem: Any): Set[Any] = this
    def iterator(): Iterator[Any] = Iterator.empty
    override def foreach[U](f: Any => U): Unit = ()
  }
  private[collection] def emptyInstance: Set[Any] = EmptySet

  /** An optimized representation for immutable sets of size 1 */
  @SerialVersionUID(1233385750652442003L)
  final class Set1[A] private[collection] (elem1: A) extends Set[A] with SmallSet[A] with Serializable {
    override def size: Int = 1
    def contains(elem: A): Boolean = elem == elem1
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set2(elem1, elem)
    def excl(elem: A): Set[A] =
      if (elem == elem1) Set.empty
      else this
    def iterator(): Iterator[A] = Iterator.single(elem1)
    override def foreach[U](f: A => U): Unit = f(elem1)
    override def exists(p: A => Boolean): Boolean = p(elem1)
    override def forall(p: A => Boolean): Boolean = p(elem1)
    override def find(p: A => Boolean): Option[A] =
      if (p(elem1)) Some(elem1)
      else None
    override def head: A = elem1
    override def tail: Set[A] = Set.empty
  }

  /** An optimized representation for immutable sets of size 2 */
  @SerialVersionUID(-6443011234944830092L)
  final class Set2[A] private[collection] (elem1: A, elem2: A) extends Set[A] with SmallSet[A] with Serializable {
    override def size: Int = 2
    def contains(elem: A): Boolean = elem == elem1 || elem == elem2
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    def iterator(): Iterator[A] = (elem1 :: elem2 :: Nil).iterator()
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2)
    }
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set1(elem2)
  }

  /** An optimized representation for immutable sets of size 3 */
  @SerialVersionUID(-3590273538119220064L)
  final class Set3[A] private[collection] (elem1: A, elem2: A, elem3: A) extends Set[A] with SmallSet[A] with Serializable {
    override def size: Int = 3
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set4(elem1, elem2, elem3, elem)
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set2(elem2, elem3)
      else if (elem == elem2) new Set2(elem1, elem3)
      else if (elem == elem3) new Set2(elem1, elem2)
      else this
    def iterator(): Iterator[A] = (elem1 :: elem2 :: elem3 :: Nil).iterator()
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3)
    }
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set2(elem2, elem3)
  }

  /** An optimized representation for immutable sets of size 4 */
  @SerialVersionUID(-3622399588156184395L)
  final class Set4[A] private[collection] (elem1: A, elem2: A, elem3: A, elem4: A) extends Set[A] with SmallSet[A] with Serializable {
    override def size: Int = 4
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else HashSet.empty[A] + elem1 + elem2 + elem3 + elem4 + elem
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    def iterator(): Iterator[A] = (elem1 :: elem2 :: elem3 :: elem4 :: Nil).iterator()
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3) || p(elem4)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3) && p(elem4)
    }
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else if (p(elem4)) Some(elem4)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set3(elem2, elem3, elem4)
  }

}
