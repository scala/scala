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
import annotation.unchecked.uncheckedVariance

/**
 * @since 2.8
 */
trait SubtractableMethods[A, +This <: Subtractable[A, This]] {
  def -(elem: A): This
  def -(elem1: A, elem2: A, elems: A*): This
  def --(xs: TraversableOnce[A]): This
}

/**
 * @since 2.8
 */
trait SetMethods[A, +This <: SetLike[A, This] with Set[A]]
          extends IterableMethods[A, This]
             with SubtractableMethods[A, This] {
  self: Set[A] =>

  // abstract
  def empty: This
  def contains(elem: A): Boolean
  def + (elem: A): This
  def - (elem: A): This

  // concrete
  def & (that: Set[A]): This
  def &~ (that: Set[A]): This
  def + (elem1: A, elem2: A, elems: A*): This
  def apply(elem: A): Boolean
  def diff(that: Set[A]): This
  def intersect(that: Set[A]): This
  def subsetOf(that: Set[A]): Boolean
  def subsets(len: Int): Iterator[This]
  def subsets: Iterator[This]
  def union(that: Set[A]): This
  def | (that: Set[A]): This
}
