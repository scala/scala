/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import generic._

/** The base trait of all builders.
 *  A builder lets one construct a collection incrementally, by adding
 *  elements to the builder with += and then converting to the required
 *  collection type with `result`.
 *
 *  @tparam  Elem  the type of elements that get added to the builder.
 *  @tparam  To    the type of collection that it produced.
 *
 *  @since 2.8
 */
trait Builder[-Elem, +To] extends Growable[Elem] {

  /** Adds a single element to the builder.
   *  @param elem the element to be added.
   *  @return the builder itself.
   */
  def +=(elem: Elem): this.type

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear()

  /** Produces a collection from the added elements.
   *  The builder's contents are undefined after this operation.
   *  @return a collection containing the elements added to this builder.
   */
  def result(): To

  /** Gives a hint how many elements are expected to be added
   *  when the next `result` is called. Some builder classes
   *  will optimize their representation based on the hint. However,
   *  builder implementations are still required to work correctly even if the hint is
   *  wrong, i.e. a different number of elements is added.
   *
   *  @param size  the hint how many elements will be added.
   */
  def sizeHint(size: Int) {}

  /** Creates a new builder by applying a transformation function to
   *  the results of this builder.
   *  @param  f     the transformation function.
   *  @tparam NewTo the type of collection returned by `f`.
   *  @return a new builder which is the same as the current builder except
   *          that a transformation function is applied to this builder's result.
   */
  def mapResult[NewTo](f: To => NewTo): Builder[Elem, NewTo] =
    new Builder[Elem, NewTo] with Proxy {
      val self = Builder.this
      def +=(x: Elem): this.type = { self += x; this }
      def clear() = self.clear()
      override def ++=(xs: Iterator[Elem]): this.type = { self ++= xs; this }
      override def ++=(xs:scala.collection.Traversable[Elem]): this.type = { self ++= xs; this }
      def result: NewTo = f(self.result)
    }
}

