/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._

/** A template trait for all sequences which may be traversed
 *  in parallel.
 *
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenSeqLike[+T, +Repr] extends GenIterableLike[T, Repr] with Equals with Parallelizable[T, parallel.ParSeq[T]] {

  def apply(idx: Int): T

  def length: Int

  def segmentLength(p: T => Boolean, from: Int): Int

  def prefixLength(p: T => Boolean): Int

  def indexWhere(p: T => Boolean, from: Int): Int

  def indexWhere(p: T => Boolean): Int

  def findIndexOf(p: T => Boolean): Int

  def indexOf[U >: T](elem: U): Int

  def indexOf[U >: T](elem: U, from: Int): Int

  def lastIndexWhere(p: T => Boolean, end: Int): Int

  def reverse: Repr

  def reverseMap[S, That](f: T => S)(implicit bf: CanBuildFrom[Repr, S, That]): That

  def startsWith[S](that: GenSeq[S]): Boolean

  def startsWith[S](that: GenSeq[S], offset: Int): Boolean

  def endsWith[S](that: GenSeq[S]): Boolean

  def patch[U >: T, That](from: Int, patch: GenSeq[U], replaced: Int)(implicit bf: CanBuildFrom[Repr, U, That]): That

  def updated[U >: T, That](index: Int, elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That

  def +:[U >: T, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That

  def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That

  def padTo[U >: T, That](len: Int, elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That

  def corresponds[S](that: GenSeq[S])(p: (T, S) => Boolean): Boolean

  def toSeq: GenSeq[T]


  /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *  $willNotTerminateInf
   *
   *  Another way to express this
   *  is that `xs union ys` computes the order-presevring multi-set union of `xs` and `ys`.
   *  `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
   *
   *  $willNotTerminateInf
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *  @usecase def union(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                followed by all elements of `that`.
   */
  def union[U >: T, That](that: GenSeq[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = this ++ that

  /** Computes the multiset difference between this $coll and another sequence.
   *  $willNotTerminateInf
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *  @usecase def diff(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   */
  def diff[U >: T](that: GenSeq[U]): Repr

  /** Computes the multiset intersection between this $coll and another sequence.
   *  $mayNotTerminateInf
   *
   *  @param that   the sequence of elements to intersect with.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *  @usecase def intersect(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   */
  def intersect[U >: T](that: GenSeq[U]): Repr

  /** Builds a new $coll from this $coll without any duplicate elements.
   *  $willNotTerminateInf
   *
   *  @return  A new $coll which contains the first occurrence of every element of this $coll.
   */
  def distinct: Repr

  /** Hashcodes for $Coll produce a value from the hashcodes of all the
   *  elements of the $coll.
   */
  override def hashCode() = {
    val h = new util.MurmurHash[T](Seq.hashSeed)
    seq.foreach(h)
    h.hash
  }

  /** The equals method for arbitrary sequences. Compares this sequence to
   *  some other object.
   *  @param    that  The object to compare the sequence to
   *  @return   `true` if `that` is a sequence that has the same elements as
   *            this sequence in the same order, `false` otherwise
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenSeq[_] => (that canEqual this) && (this sameElements that)
    case _               => false
  }

}
