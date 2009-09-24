/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.interfaces

import scala.collection._
import generic._
import mutable.Buffer
import scala.reflect.ClassManifest

trait SequenceMethods[+A, +This <: SequenceTemplate[A, This] with Sequence[A]] extends IterableMethods[A, This]
{
  // abstract
  def apply(idx: Int): A
  def length: Int

  def contains(elem: Any): Boolean
  def diff[B >: A, That](that: Sequence[B]): This
  def endsWith[B](that: Sequence[B]): Boolean
  def indexOfSeq[B >: A](that: Sequence[B]): Int
  def indexOfSeq[B >: A](that: Sequence[B], fromIndex: Int): Int
  def indexOf[B >: A](elem: B): Int
  def indexOf[B >: A](elem: B, from: Int): Int
  def indexWhere(p: A => Boolean): Int
  def indexWhere(p: A => Boolean, from: Int): Int
  def indices: Range
  def intersect[B >: A, That](that: Sequence[B]): This
  def isDefinedAt(x: Int): Boolean
  def lastIndexOfSeq[B >: A](that: Sequence[B]): Int
  def lastIndexOfSeq[B >: A](that: Sequence[B], fromIndex: Int): Int
  def lastIndexOf[B >: A](elem: B): Int
  def lastIndexOf[B >: A](elem: B, end: Int): Int
  def lastIndexWhere(p: A => Boolean): Int
  def lastIndexWhere(p: A => Boolean, end: Int): Int
  def lengthCompare(len: Int): Int
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: BuilderFactory[B, That, This]): That
  def patch[B >: A, That](from: Int, patch: Sequence[B], replaced: Int)(implicit bf: BuilderFactory[B, That, This]): That
  def prefixLength(p: A => Boolean): Int
  def removeDuplicates: This
  def reverse: This
  def reverseIterator: Iterator[A]
  def segmentLength(p: A => Boolean, from: Int): Int
  def slice(from: Int): Sequence[A]
  def startsWith[B](that: Sequence[B]): Boolean
  def startsWith[B](that: Sequence[B], offset: Int): Boolean
  def union[B >: A, That](that: Sequence[B])(implicit bf: BuilderFactory[B, That, This]): That

  override def view: SequenceView[A, This]
  override def view(from: Int, until: Int): SequenceView[A, This]
}
