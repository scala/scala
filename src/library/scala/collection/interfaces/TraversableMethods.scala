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
trait TraversableMethods[+A, +This <: TraversableLike[A, This]] extends TraversableOnceMethods[A] {
  self: Traversable[A] =>

  // maps/iteration
  def flatMap[B, That](f: A => TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That
  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[This, B, That]): That
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[This, B, That]): That
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[This, B, That]): That

  // new collections
  def ++[B >: A, That](xs: TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That

  // element retrieval
  def head: A
  def headOption: Option[A]
  def last: A
  def lastOption: Option[A]

  // subcollections
  def drop(n: Int): Traversable[A]
  def dropWhile(p: A => Boolean): Traversable[A]
  def filter(p: A => Boolean): Traversable[A]
  def filterNot(p: A => Boolean): Traversable[A]
  def withFilter(p: A => Boolean): FilterMonadic[A, Traversable[A]]
  def init: Traversable[A]
  def slice(from: Int, until: Int): Traversable[A]
  def tail: Traversable[A]
  def take(n: Int): Traversable[A]
  def takeWhile(p: A => Boolean): Traversable[A]

  // subdivisions
  def groupBy[K](f: A => K): Map[K, Traversable[A]]
  def partition(p: A => Boolean): (Traversable[A], Traversable[A])
  def span(p: A => Boolean): (Traversable[A], Traversable[A])
  def splitAt(n: Int): (Traversable[A], Traversable[A])

  // info
  def count(p: A => Boolean): Int
  def size: Int
  def stringPrefix: String

  // views
  def view: TraversableView[A, This]
  def view(from: Int, until: Int): TraversableView[A, This]
}
