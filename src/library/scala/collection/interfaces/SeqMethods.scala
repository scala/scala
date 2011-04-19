/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
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
trait SeqMethods[+A, +This <: SeqLike[A, This] with Seq[A]] extends IterableMethods[A, This] {
  self: Seq[A] =>

  // abstract
  def apply(idx: Int): A
  def length: Int

  // concrete
  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That
  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That
  def combinations(n: Int): Iterator[This]
  def contains(elem: Any): Boolean
  def containsSlice[B](that: Seq[B]): Boolean
  def corresponds[B](that: Seq[B])(p: (A,B) => Boolean): Boolean
  def diff[B >: A, That](that: Seq[B]): This
  def distinct: This
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
  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[This, B, That]): That
  def permutations: Iterator[This]
  def prefixLength(p: A => Boolean): Int
  def reverse: This
  def reverseIterator: Iterator[A]
  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That
  def segmentLength(p: A => Boolean, from: Int): Int
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): This
  def sortWith(lt: (A, A) => Boolean): This
  def sorted[B >: A](implicit ord: Ordering[B]): This
  def startsWith[B](that: Seq[B]): Boolean
  def startsWith[B](that: Seq[B], offset: Int): Boolean
  def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[This, B, That]): That
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That

  // refinements
  def view: SeqView[A, This]
  def view(from: Int, until: Int): SeqView[A, This]
}
