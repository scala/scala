/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.util.hashing.MurmurHash3
import java.lang.String

import scala.annotation.nowarn

/** Base trait for set collections.
  */
trait Set[A]
  extends Iterable[A]
    with SetOps[A, Set, Set[A]]
    with Equals
    with IterableFactoryDefaults[A, Set] {

  def canEqual(that: Any) = true

  /**
   * Equality of sets is implemented using the lookup method [[contains]]. This method returns `true` if
   *   - the argument `that` is a `Set`,
   *   - the two sets have the same [[size]], and
   *   - for every `element` this set, `other.contains(element) == true`.
   *
   * The implementation of `equals` checks the [[canEqual]] method, so subclasses of `Set` can narrow down the equality
   * to specific set types. The `Set` implementations in the standard library can all be compared, their `canEqual`
   * methods return `true`.
   *
   * Note: The `equals` method only respects the equality laws (symmetry, transitivity) if the two sets use the same
   * element equivalence function in their lookup operation. For example, the element equivalence operation in a
   * [[scala.collection.immutable.TreeSet]] is defined by its ordering. Comparing a `TreeSet` with a `HashSet` leads
   * to unexpected results if `ordering.equiv(e1, e2)` (used for lookup in `TreeSet`) is different from `e1 == e2`
   * (used for lookup in `HashSet`).
   *
   * {{{
   *   scala> import scala.collection.immutable._
   *   scala> val ord: Ordering[String] = _ compareToIgnoreCase _
   *
   *   scala> TreeSet("A")(ord) == HashSet("a")
   *   val res0: Boolean = false
   *
   *   scala> HashSet("a") == TreeSet("A")(ord)
   *   val res1: Boolean = true
   * }}}
   *
   *
   * @param that The set to which this set is compared
   * @return `true` if the two sets are equal according to the description
   */
  override def equals(that: Any): Boolean =
    (this eq that.asInstanceOf[AnyRef]) || (that match {
      case set: Set[A @unchecked] if set.canEqual(this) =>
        (this.size == set.size) && {
          try this.subsetOf(set)
          catch { case _: ClassCastException => false } // PR #9565 / scala/bug#12228
        }
      case _ =>
        false
    })

  override def hashCode(): Int = MurmurHash3.setHash(this)

  override def iterableFactory: IterableFactory[Set] = Set

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "Set"

  override def toString(): String = super[Iterable].toString() // Because `Function1` overrides `toString` too
}

/** Base trait for set operations
  *
  * @define coll set
  * @define Coll `Set`
  */
trait SetOps[A, +CC[_], +C <: SetOps[A, CC, C]]
  extends IterableOps[A, CC, C]
     with (A => Boolean) {

  def contains(elem: A): Boolean

  /** Tests if some element is contained in this set.
    *
    *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
    *  @param elem the element to test for membership.
    *  @return  `true` if `elem` is contained in this set, `false` otherwise.
    */
  @`inline` final def apply(elem: A): Boolean = this.contains(elem)

  /** Tests whether this set is a subset of another set.
    *
    *  @param that  the set to test.
    *  @return     `true` if this set is a subset of `that`, i.e. if
    *              every element of this set is also an element of `that`.
    */
  def subsetOf(that: Set[A]): Boolean = this.forall(that)

  /** An iterator over all subsets of this set of the given size.
    *  If the requested size is impossible, an empty iterator is returned.
    *
    *  @param len  the size of the subsets.
    *  @return     the iterator.
    */
  def subsets(len: Int): Iterator[C] = {
    if (len < 0 || len > size) Iterator.empty
    else new SubsetsItr(this.to(IndexedSeq), len)
  }

  /** An iterator over all subsets of this set.
    *
    *  @return     the iterator.
    */
  def subsets(): Iterator[C] = new AbstractIterator[C] {
    private[this] val elms = SetOps.this.to(IndexedSeq)
    private[this] var len = 0
    private[this] var itr: Iterator[C] = Iterator.empty

    def hasNext = len <= elms.size || itr.hasNext
    def next() = {
      if (!itr.hasNext) {
        if (len > elms.size) Iterator.empty.next()
        else {
          itr = new SubsetsItr(elms, len)
          len += 1
        }
      }

      itr.next()
    }
  }

  /** An Iterator including all subsets containing exactly len elements.
    *  If the elements in 'This' type is ordered, then the subsets will also be in the same order.
    *  ListSet(1,2,3).subsets => {{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}
    *
    *  $willForceEvaluation
    *
    */
  private class SubsetsItr(elms: IndexedSeq[A], len: Int) extends AbstractIterator[C] {
    private[this] val idxs = Array.range(0, len+1)
    private[this] var _hasNext = true
    idxs(len) = elms.size

    def hasNext = _hasNext
    @throws[NoSuchElementException]
    def next(): C = {
      if (!hasNext) Iterator.empty.next()

      val buf = newSpecificBuilder
      idxs.slice(0, len) foreach (idx => buf += elms(idx))
      val result = buf.result()

      var i = len - 1
      while (i >= 0 && idxs(i) == idxs(i+1)-1) i -= 1

      if (i < 0) _hasNext = false
      else {
        idxs(i) += 1
        for (j <- (i+1) until len)
          idxs(j) = idxs(j-1) + 1
      }

      result
    }
  }

  /** Computes the intersection between this set and another set.
    *
    *  @param   that  the set to intersect with.
    *  @return  a new set consisting of all elements that are both in this
    *  set and in the given set `that`.
    */
  def intersect(that: Set[A]): C = this.filter(that)

  /** Alias for `intersect` */
  @`inline` final def & (that: Set[A]): C = intersect(that)

  /** Computes the difference of this set and another set.
    *
    *  @param that the set of elements to exclude.
    *  @return     a set containing those elements of this
    *              set that are not also contained in the given set `that`.
    */
  def diff(that: Set[A]): C

  /** Alias for `diff` */
  @`inline` final def &~ (that: Set[A]): C = this diff that

  @deprecated("Consider requiring an immutable Set", "2.13.0")
  def -- (that: IterableOnce[A]): C = {
    val toRemove = that.iterator.to(immutable.Set)
    fromSpecific(view.filterNot(toRemove))
  }

  @deprecated("Consider requiring an immutable Set or fall back to Set.diff", "2.13.0")
  def - (elem: A): C = diff(Set(elem))

  @deprecated("Use &- with an explicit collection argument instead of - with varargs", "2.13.0")
  def - (elem1: A, elem2: A, elems: A*): C = diff(elems.toSet + elem1 + elem2)

  /** Creates a new $coll by adding all elements contained in another collection to this $coll, omitting duplicates.
    *
    * This method takes a collection of elements and adds all elements, omitting duplicates, into $coll.
    *
    * Example:
    *  {{{
    *    scala> val a = Set(1, 2) concat Set(2, 3)
    *    a: scala.collection.immutable.Set[Int] = Set(1, 2, 3)
    *  }}}
    *
    *  @param that     the collection containing the elements to add.
    *  @return a new $coll with the given elements added, omitting duplicates.
    */
  def concat(that: collection.IterableOnce[A]): C = fromSpecific(that match {
    case that: collection.Iterable[A] => new View.Concat(this, that)
    case _ => iterator.concat(that.iterator)
  })

  @deprecated("Consider requiring an immutable Set or fall back to Set.union", "2.13.0")
  def + (elem: A): C = fromSpecific(new View.Appended(this, elem))

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  def + (elem1: A, elem2: A, elems: A*): C = fromSpecific(new View.Concat(new View.Appended(new View.Appended(this, elem1), elem2), elems))

  /** Alias for `concat` */
  @`inline` final def ++ (that: collection.IterableOnce[A]): C = concat(that)

  /** Computes the union between of set and another set.
    *
    *  @param   that  the set to form the union with.
    *  @return  a new set consisting of all elements that are in this
    *  set or in the given set `that`.
    */
  @`inline` final def union(that: Set[A]): C = concat(that)

  /** Alias for `union` */
  @`inline` final def | (that: Set[A]): C = concat(that)
}

/**
  * $factoryInfo
  * @define coll set
  * @define Coll `Set`
  */
@SerialVersionUID(3L)
object Set extends IterableFactory.Delegate[Set](immutable.Set)

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]
