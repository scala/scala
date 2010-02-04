/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package interfaces

import generic._
import mutable.Buffer
import scala.reflect.ClassManifest

/**
 * @since 2.8
 */
trait SeqMethods[+A, +This <: SeqLike[A, This] with Seq[A]] extends IterableMethods[A, This]
{
  // abstract
  def apply(idx: Int): A
  def length: Int

  def contains(elem: Any): Boolean
  def diff[B >: A, That](that: Seq[B]): This
  def endsWith[B](that: Seq[B]): Boolean
  def indexOfSlice[B >: A](that: Seq[B]): Int
  def indexOfSlice[B >: A](that: Seq[B], fromIndex: Int): Int
  def indexOf[B >: A](elem: B): Int
  def indexOf[B >: A](elem: B, from: Int): Int
  def indexWhere(p: A => Boolean): Int
  def indexWhere(p: A => Boolean, from: Int): Int
  def indices: Range
  def intersect[B >: A, That](that: Seq[B]): This
  def isDefinedAt(x: Int): Boolean
  def lastIndexOfSlice[B >: A](that: Seq[B]): Int
  def lastIndexOfSlice[B >: A](that: Seq[B], fromIndex: Int): Int
  def lastIndexOf[B >: A](elem: B): Int
  def lastIndexOf[B >: A](elem: B, end: Int): Int
  def lastIndexWhere(p: A => Boolean): Int
  def lastIndexWhere(p: A => Boolean, end: Int): Int
  def lengthCompare(len: Int): Int
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That
  def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[This, B, That]): That
  def prefixLength(p: A => Boolean): Int
  def distinct: This
  def reverse: This
  def reverseIterator: Iterator[A]
  def segmentLength(p: A => Boolean, from: Int): Int
  def startsWith[B](that: Seq[B]): Boolean
  def startsWith[B](that: Seq[B], offset: Int): Boolean
  def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[This, B, That]): That

  override def view: SeqView[A, This]
  override def view(from: Int, until: Int): SeqView[A, This]
}
