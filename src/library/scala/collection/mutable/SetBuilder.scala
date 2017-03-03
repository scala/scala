/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

/** The canonical builder for mutable Sets.
 *
 *  @tparam A      The type of the elements that will be contained in this set.
 *  @tparam Coll   The type of the actual collection this set builds.
 *  @param empty   The empty element of the collection.
 *  @since 2.8
 */
class SetBuilder[A, Coll <: scala.collection.Set[A]
with scala.collection.SetLike[A, Coll]](empty: Coll)
extends ReusableBuilder[A, Coll] {
  protected var elems: Coll = empty
  def +=(x: A): this.type = { elems = elems + x; this }
  def clear() { elems = empty }
  def result: Coll = elems
}
