/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

/** A template trait for mutable sequences of type `mutable.Seq[A]`.
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 */
trait SeqLike[A, +This <: SeqLike[A, This] with Seq[A]]
  extends scala.collection.SeqLike[A, This]
     with Cloneable[This] {
  self =>

  /** Replaces element at given index with a new value.
   *
   *  @param n       the index of the element to replace.
   *  @param lem     the new value.
   *  @throws   IndexOutofBoundsException if the index is not valid.
   */
  def update(idx: Int, elem: A)
}
