/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

// Methods could be printed by  cat SeqLike.scala | egrep '^  (override )?def'


/** This trait implements a proxy for sequences. It forwards
 *  all calls to a different sequence.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait SeqProxyLike[+A, +This <: SeqLike[A, This] with Seq[A]] extends SeqLike[A, This] with IterableProxyLike[A, This] {
  override def length: Int = self.length
  override def apply(idx: Int): A = self.apply(idx)
  override def lengthCompare(len: Int): Int = self.lengthCompare(len)
  override def size = self.size
  override def isDefinedAt(x: Int): Boolean = self.isDefinedAt(x)
  override def segmentLength(p: A => Boolean, from: Int): Int = self.segmentLength(p, from)
  override def prefixLength(p: A => Boolean) = self.prefixLength(p)
  override def indexWhere(p: A => Boolean): Int = self.indexWhere(p)
  override def indexWhere(p: A => Boolean, from: Int): Int = self.indexWhere(p, from)
  override def findIndexOf(p: A => Boolean): Int = self.indexWhere(p)
  override def indexOf[B >: A](elem: B): Int = self.indexOf(elem)
  override def indexOf[B >: A](elem: B, from: Int): Int = self.indexOf(elem, from)
  override def lastIndexOf[B >: A](elem: B): Int = self.lastIndexOf(elem)
  override def lastIndexOf[B >: A](elem: B, end: Int): Int = self.lastIndexWhere(elem ==, end)
  override def lastIndexWhere(p: A => Boolean): Int = self.lastIndexWhere(p, length - 1)
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = self.lastIndexWhere(p)
  override def reverse: This = self.reverse
  override def reverseIterator: Iterator[A] = self.reverseIterator
  override def startsWith[B](that: Seq[B], offset: Int): Boolean = self.startsWith(that, offset)
  override def startsWith[B](that: Seq[B]): Boolean = self.startsWith(that)
  override def endsWith[B](that: Seq[B]): Boolean = self.endsWith(that)
  override def indexOfSlice[B >: A](that: Seq[B]): Int = self.indexOfSlice(that)
  override def contains(elem: Any): Boolean = self.contains(elem)
  override def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[This, B, That]): That = self.union(that)(bf)
  override def diff[B >: A, That](that: Seq[B]): This = self.diff(that)
  override def intersect[B >: A, That](that: Seq[B]): This = self.intersect(that)
  override def removeDuplicates: This = self.removeDuplicates
  override def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[This, B, That]): That = self.patch(from, patch, replaced)(bf)
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That = self.padTo(len, elem)(bf)
  override def indices: Range = self.indices
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
  override def equalsWith[B](that: Seq[B])(f: (A,B) => Boolean): Boolean = (self zip that) forall { case (x,y) => f(x,y) }
  override def containsSlice[B](that: Seq[B]): Boolean = self.indexOfSlice(that) != -1
}
