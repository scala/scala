/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import immutable.{List, Nil}

/** A builder that constructs its result lazily. Iterators or iterables to
 *  be added to this builder with `++=` are not evaluated until `result` is called.
 *
 *  @since 2.8
 */
abstract class LazyBuilder[Elem, +To] extends Builder[Elem, To] {
  /** The different segments of elements to be added to the builder, represented as iterators */
  protected var parts = new ListBuffer[scala.collection.Traversable[Elem]]
  def +=(x: Elem): this.type = { parts += List(x); this }
  override def ++=(xs: Iterator[Elem]): this.type = { parts += xs.toStream; this }
  override def ++=(xs: scala.collection.Traversable[Elem]): this.type = { parts += xs; this }
  def result(): To
  def clear() { parts.clear() }
}
