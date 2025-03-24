/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.collection.immutable.Set.Set4
import scala.collection.mutable.{Builder, ReusableBuilder}

/** Base trait for immutable set collections */
trait Set[A] extends Iterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]]
    with IterableFactoryDefaults[A, Set] {
  override def iterableFactory: IterableFactory[Set] = Set
}

/** Base trait for immutable set operations
  *
  * @define coll immutable set
  * @define Coll `immutable.Set`
  */
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
  override final def + (elem: A): C = incl(elem) // like in collection.Set but not deprecated

  /** Creates a new set with a given element removed from this set.
    *
    *  @param elem the element to be removed
    *  @return a new set that contains all elements of this set but that does not
    *          contain `elem`.
    */
  def excl(elem: A): C

  /** Alias for `excl` */
  @`inline` final override def - (elem: A): C = excl(elem)

  def diff(that: collection.Set[A]): C =
    foldLeft(empty)((result, elem) => if (that contains elem) result else result + elem)

  /** Creates a new $coll from this $coll by removing all elements of another
    *  collection.
    *
    *  @param that the collection containing the elements to remove.
    *  @return a new $coll with the given elements removed, omitting duplicates.
    */
  def removedAll(that: IterableOnce[A]): C = that.iterator.foldLeft[C](coll)(_ - _)

  /** Alias for removedAll */
  override final def -- (that: IterableOnce[A]): C = removedAll(that)
}

trait StrictOptimizedSetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with collection.StrictOptimizedSetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def concat(that: collection.IterableOnce[A]): C = {
    var result: C = coll
    val it = that.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}

/**
  * $factoryInfo
  * @define coll immutable set
  * @define Coll `immutable.Set`
  */
@SerialVersionUID(3L)
object Set extends IterableFactory[Set] {

  def empty[A]: Set[A] = EmptySet.asInstanceOf[Set[A]]

  def from[E](it: collection.IterableOnce[E]): Set[E] =
    it match {
      case _ if it.knownSize == 0 => empty[E]
      // Since IterableOnce[E] launders the variance of E,
      // identify only our implementations which can be soundly substituted.
      // It's not sufficient to match `SortedSet[E]` to rebuild and `Set[E]` to retain.
      case s: HashSet[E] => s
      case s: ListSet[E] => s
      case s: Set1[E]    => s
      case s: Set2[E]    => s
      case s: Set3[E]    => s
      case s: Set4[E]    => s
      case s: HashMap[E @unchecked, _]#HashKeySet => s
      case s: MapOps[E, Any, Map, Map[E, Any]]#ImmutableKeySet @unchecked => s
      // We also want `SortedSet` (and subclasses, such as `BitSet`)
      // to rebuild themselves, to avoid element type widening issues.
      case _ => newBuilder[E].addAll(it).result()
    }

  def newBuilder[A]: Builder[A, Set[A]] = new SetBuilderImpl[A]

  /** An optimized representation for immutable empty sets */
  @SerialVersionUID(3L)
  private object EmptySet extends AbstractSet[Any] with Serializable {
    override def size: Int = 0
    override def isEmpty = true
    override def knownSize: Int = size
    override def filter(pred: Any => Boolean): Set[Any] = this
    override def filterNot(pred: Any => Boolean): Set[Any] = this
    override def removedAll(that: IterableOnce[Any]): Set[Any] = this
    override def diff(that: collection.Set[Any]): Set[Any] = this
    override def subsetOf(that: collection.Set[Any]): Boolean = true
    override def intersect(that: collection.Set[Any]): Set[Any] = this
    override def view: View[Any] = View.empty
    def contains(elem: Any): Boolean = false
    def incl(elem: Any): Set[Any] = new Set1(elem)
    def excl(elem: Any): Set[Any] = this
    def iterator: Iterator[Any] = Iterator.empty
    override def foreach[U](f: Any => U): Unit = ()
  }
  private[collection] def emptyInstance: Set[Any] = EmptySet

  @SerialVersionUID(3L)
  private abstract class SetNIterator[A](n: Int) extends AbstractIterator[A] with Serializable {
    private[this] var current = 0
    private[this] var remainder = n
    override def knownSize: Int = remainder
    def hasNext = remainder > 0
    def apply(i: Int): A
    def next(): A =
      if (hasNext) {
        val r = apply(current)
        current += 1
        remainder -= 1
        r
      } else Iterator.empty.next()

    override def drop(n: Int): Iterator[A] = {
      if (n > 0) {
        current += n
        remainder = Math.max(0, remainder - n)
      }
      this
    }
  }

  /** An optimized representation for immutable sets of size 1 */
  @SerialVersionUID(3L)
  final class Set1[A] private[collection] (elem1: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    override def size: Int = 1
    override def isEmpty = false
    override def knownSize: Int = size
    def contains(elem: A): Boolean = elem == elem1
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set2(elem1, elem)
    def excl(elem: A): Set[A] =
      if (elem == elem1) Set.empty
      else this
    def iterator: Iterator[A] = Iterator.single(elem1)
    override def foreach[U](f: A => U): Unit = f(elem1)
    override def exists(p: A => Boolean): Boolean = p(elem1)
    override def forall(p: A => Boolean): Boolean = p(elem1)
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] =
      if (pred(elem1) != isFlipped) this else Set.empty

    override def find(p: A => Boolean): Option[A] =
      if (p(elem1)) Some(elem1)
      else None
    override def head: A = elem1
    override def tail: Set[A] = Set.empty
  }

  /** An optimized representation for immutable sets of size 2 */
  @SerialVersionUID(3L)
  final class Set2[A] private[collection] (elem1: A, elem2: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    override def size: Int = 2
    override def isEmpty = false
    override def knownSize: Int = size
    def contains(elem: A): Boolean = elem == elem1 || elem == elem2
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2)
    }
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1; n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => this
      }
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
  @SerialVersionUID(3L)
  final class Set3[A] private[collection] (elem1: A, elem2: A, elem3: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    override def size: Int = 3
    override def isEmpty = false
    override def knownSize: Int = size
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
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3)
    }
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                             n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2; n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => this
      }
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
  @SerialVersionUID(3L)
  final class Set4[A] private[collection] (elem1: A, elem2: A, elem3: A, elem4: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    override def size: Int = 4
    override def isEmpty = false
    override def knownSize: Int = size
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
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 case 3 => elem4 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3) || p(elem4)
    }
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3) && p(elem4)
    }
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2, r3: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                                                         n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2;                             n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3 else             r3 = elem3; n += 1}
      if (pred(elem4) != isFlipped) { if (n == 0) r1 = elem4 else if (n == 1) r2 = elem4 else if (n == 2) r3 = elem4; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => new Set3(r1, r2, r3)
        case 4 => this
      }
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

    private[immutable] def buildTo(builder: Builder[A, Set[A]]): builder.type =
      builder.addOne(elem1).addOne(elem2).addOne(elem3).addOne(elem4)
  }
}

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends scala.collection.AbstractSet[A] with Set[A]

/** Builder for Set.
  * $multipleResults
  */
private final class SetBuilderImpl[A] extends ReusableBuilder[A, Set[A]] {
  private[this] var elems: Set[A] = Set.empty
  private[this] var switchedToHashSetBuilder: Boolean = false
  private[this] var hashSetBuilder: HashSetBuilder[A] = _

  override def clear(): Unit = {
    elems = Set.empty
    if (hashSetBuilder != null) {
      hashSetBuilder.clear()
    }
    switchedToHashSetBuilder = false
  }

  override def result(): Set[A] =
    if (switchedToHashSetBuilder) hashSetBuilder.result() else elems

  def addOne(elem: A) = {
    if (switchedToHashSetBuilder) {
      hashSetBuilder.addOne(elem)
    } else if (elems.size < 4) {
      elems = elems + elem
    } else {
      // assert(elems.size == 4)
      if (elems.contains(elem)) {
        () // do nothing
      } else {
        switchedToHashSetBuilder = true
        if (hashSetBuilder == null) {
          hashSetBuilder = new HashSetBuilder
        }
        elems.asInstanceOf[Set4[A]].buildTo(hashSetBuilder)
        hashSetBuilder.addOne(elem)
      }
    }

    this
  }

  override def addAll(xs: IterableOnce[A]): this.type =
    if (switchedToHashSetBuilder) {
      hashSetBuilder.addAll(xs)
      this
    } else {
      super.addAll(xs)
    }
}
