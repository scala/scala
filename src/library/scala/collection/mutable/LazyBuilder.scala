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

/** A builder that constructs its result lazily. Iterators or iterables to
 *  be added to this builder with `++=` are not evaluated until `result` is called.
 *
 *  This builder can be reused.
 *
 *  @since 2.8
 *
 *  @tparam Elem    type of the elements for this builder.
 *  @tparam To      type of the collection this builder builds.
 */
abstract class LazyBuilder[Elem, +To] extends ReusableBuilder[Elem, To] {
  /** The different segments of elements to be added to the builder, represented as iterators */
  protected var parts = new ListBuffer[TraversableOnce[Elem]]
  def +=(x: Elem): this.type = { parts += List(x); this }
  override def ++=(xs: TraversableOnce[Elem]): this.type = { parts += xs ; this }
  def result(): To
  def clear() { parts.clear() }
}
