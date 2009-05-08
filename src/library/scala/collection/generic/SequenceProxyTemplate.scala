/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scala.collection.generic

import collection.mutable.Buffer

// Methods could be printed by  cat SequenceTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait SequenceProxyTemplate[+A, +This <: SequenceTemplate[A, This] with Sequence[A]] extends SequenceTemplate[A, This] with IterableProxyTemplate[A, This] {
  override def length: Int = self.length
  override def apply(idx: Int): A = self.apply(idx)
  override def lengthCompare(len: Int): Int = self.lengthCompare(len)
  override def isDefinedAt(x: Int): Boolean = self.isDefinedAt(x)
  override def zip[A1 >: A, B, That](that: Sequence[B])(implicit bf: BuilderFactory[(A1, B), That, This]): That = self.zip[A1, B, That](that)(bf)
  override def zipAll[B, A1 >: A, That](that: Sequence[B], thisElem: A1, thatElem: B)(implicit bf: BuilderFactory[(A1, B), That, This]): That = self.zipAll(that, thisElem, thatElem)(bf)
  override def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, This]): That = self.zipWithIndex(bf)
  override def segmentLength(p: A => Boolean, from: Int): Int = self.segmentLength(p, from)
  override def prefixLength(p: A => Boolean) = self.prefixLength(p)
  override def indexWhere(p: A => Boolean): Int = self.indexWhere(p)
  override def indexWhere(p: A => Boolean, from: Int): Int = self.indexWhere(p, from)
  override def indexOf[B >: A](elem: B): Int = self.indexOf(elem)
  override def indexOf[B >: A](elem: B, from: Int): Int = self.indexOf(elem, from)
  override def lastIndexOf[B >: A](elem: B): Int = self.lastIndexOf(elem)
  override def lastIndexOf[B >: A](elem: B, end: Int): Int = self.lastIndexWhere(elem ==, end)
  override def lastIndexWhere(p: A => Boolean): Int = self.lastIndexWhere(p, length - 1)
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = self.lastIndexWhere(p)
  override def reverse: This = self.reverse
  override def reversedElements: Iterator[A] = self.reversedElements
  override def startsWith[B](that: Sequence[B], offset: Int): Boolean = self.startsWith(that, offset)
  override def startsWith[B](that: Sequence[B]): Boolean = self.startsWith(that)
  override def endsWith[B](that: Sequence[B]): Boolean = self.endsWith(that)
  override def indexOfSeq[B >: A](that: Sequence[B]): Int = self.indexOfSeq(that)
  override def contains(elem: Any): Boolean = self.contains(elem)
  override def union[B >: A, That](that: Sequence[B])(implicit bf: BuilderFactory[B, That, This]): That = self.union(that)(bf)
  override def diff[B >: A, That](that: Sequence[B]): This = self.diff(that)
  override def intersect[B >: A, That](that: Sequence[B]): This = self.intersect(that)
  override def removeDuplicates: This = self.removeDuplicates
  override def patch[B >: A, That](from: Int, patch: Sequence[B], replaced: Int)(implicit bf: BuilderFactory[B, That, This]): That = self.patch(from, patch, replaced)(bf)
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: BuilderFactory[B, That, This]): That = self.padTo(len, elem)(bf)
  override def indices: Range = self.indices
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
}
