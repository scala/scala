/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection

import generic._

// Methods could be printed by  cat SeqLike.scala | egrep '^  (override )?def'


/** This trait implements a proxy for sequences. It forwards
 *  all calls to a different sequence.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait SeqProxyLike[+A, +Repr <: SeqLike[A, Repr] with Seq[A]] extends SeqLike[A, Repr] with IterableProxyLike[A, Repr] {
  override def size = self.size
  override def toSeq: Seq[A] = self.toSeq
  override def length: Int = self.length
  override def apply(idx: Int): A = self.apply(idx)
  override def lengthCompare(len: Int): Int = self.lengthCompare(len)
  override def isDefinedAt(x: Int): Boolean = self.isDefinedAt(x)
  override def segmentLength(p: A => Boolean, from: Int): Int = self.segmentLength(p, from)
  override def prefixLength(p: A => Boolean) = self.prefixLength(p)
  override def indexWhere(p: A => Boolean): Int = self.indexWhere(p)
  override def indexWhere(p: A => Boolean, from: Int): Int = self.indexWhere(p, from)
  override def indexOf[B >: A](elem: B): Int = self.indexOf(elem)
  override def indexOf[B >: A](elem: B, from: Int): Int = self.indexOf(elem, from)
  override def lastIndexOf[B >: A](elem: B): Int = self.lastIndexOf(elem)
  override def lastIndexOf[B >: A](elem: B, end: Int): Int = self.lastIndexWhere(elem == _, end)
  override def lastIndexWhere(p: A => Boolean): Int = self.lastIndexWhere(p, length - 1)
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = self.lastIndexWhere(p)
  override def reverse: Repr = self.reverse
  override def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.reverseMap(f)(bf)
  override def reverseIterator: Iterator[A] = self.reverseIterator
  override def startsWith[B](that: GenSeq[B], offset: Int): Boolean = self.startsWith(that, offset)
  override def startsWith[B](that: GenSeq[B]): Boolean = self.startsWith(that)
  override def endsWith[B](that: GenSeq[B]): Boolean = self.endsWith(that)
  override def indexOfSlice[B >: A](that: GenSeq[B]): Int = self.indexOfSlice(that)
  override def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int = self.indexOfSlice(that)
  override def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = self.lastIndexOfSlice(that)
  override def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = self.lastIndexOfSlice(that, end)
  override def containsSlice[B](that: GenSeq[B]): Boolean = self.indexOfSlice(that) != -1
  override def contains[A1 >: A](elem: A1): Boolean = self.contains(elem)
  override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = self.union(that)(bf)
  override def diff[B >: A](that: GenSeq[B]): Repr = self.diff(that)
  override def intersect[B >: A](that: GenSeq[B]): Repr = self.intersect(that)
  override def distinct: Repr = self.distinct
  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.patch(from, patch, replaced)(bf)
  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.updated(index, elem)(bf)
  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.+:(elem)(bf)
  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.:+(elem)(bf)
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.padTo(len, elem)(bf)
  override def corresponds[B](that: GenSeq[B])(p: (A,B) => Boolean): Boolean = self.corresponds(that)(p)
  override def sortWith(lt: (A, A) => Boolean): Repr = self.sortWith(lt)
  override def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = self.sortBy(f)(ord)
  override def sorted[B >: A](implicit ord: Ordering[B]): Repr = self.sorted(ord)
  override def indices: Range = self.indices
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
}


