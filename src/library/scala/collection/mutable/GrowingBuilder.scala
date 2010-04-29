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

/** The canonical builder for collections that are growable, i.e. that support an
 *  efficient `+=` method which adds an element to the collection.  It is
 *  almost identical to `AddingBuilder`, but necessitated by the existence of
 *  classes which are `Growable` but not `Addable`, which is a result of covariance
 *  interacting surprisingly with any2stringadd thus driving '+' out of the `Seq`
 *  hierarchy.  The tendrils of original sin should never be underestimated.
 *
 *  Addendum: of even greater significance is that '+' on mutable collections now
 *  creates a new collection.  This means using AddingBuilder on them will create
 *  a new intermediate collection for every element given to the builder, taking
 *  '+' from an O(1) to O(n) operation.
 *
 *  @author Paul Phillips
 *  @version 2.8
 *  @since 2.8
 *
 *  @define Coll GrowingBuilder
 *  @define coll growing builder
 */
class GrowingBuilder[Elem, To <: Growable[Elem]](empty: To) extends Builder[Elem, To] {
  protected var elems: To = empty
  def +=(x: Elem): this.type = { elems += x; this }
  def clear() { elems = empty }
  def result: To = elems
}
