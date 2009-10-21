/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

import mutable.Builder

/** A base class for builder factories
 *
 *  @since 2.8
 */
trait CanBuildFrom[-From, -Elem, +To] {

  /** Creates a new builder, using `from` as a prototype
   * the resulting Builder will build the same kind of collection
   */
  def apply(from: From): Builder[Elem, To]

  /** Creates a new builder from scratch */
  def apply(): Builder[Elem, To]
}
