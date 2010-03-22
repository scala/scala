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
 *  almost identical to AddingBuilder, but necessitated by the existence of
 *  classes which are Growable but not Addable, which is a result of covariance
 *  interacting surprisingly with any2stringadd thus driving '+' out of the Seq
 *  hierachy.  The tendrils of original sin should never be underestimated.
 *
 *  @author Paul Phillips
 *  @version 2.8
 *  @since 2.8
 */
class GrowingBuilder[Elem, To <: Growable[Elem]](empty: To) extends Builder[Elem, To] {
  protected var elems: To = empty
  def +=(x: Elem): this.type = { elems += x; this }
  def clear() { elems = empty }
  def result: To = elems
}
