/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

/** The canonical builder for collections that are addable, i.e. that support
 *  an efficient `+` method which adds an element to the collection.
 *
 *  Collections are built from their empty element using this `+` method.
 *  @param  empty   the empty element of the collection.
 *  @tparam Elem    the type of elements that get added to the builder.
 *  @tparam To      the type of the built collection.
 *
 *  @note   "efficient `+`" is not idle talk.  Do not use this on mutable collections or any others
 *  for which `+` may perform an unshared copy! See GrowingBuilder comments for more.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since 2.8
 */
@deprecated("Will be removed after scala 2.9", "2.8.0")
class AddingBuilder[Elem, To <: Addable[Elem, To] with collection.Iterable[Elem] with collection.IterableLike[Elem, To]](empty: To)
extends Builder[Elem, To] {
  protected var elems: To = empty
  def +=(x: Elem): this.type = { elems = elems + x; this }
  def clear() { elems = empty }
  def result: To = elems
}
