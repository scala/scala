/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import mutable.{Builder, AddingBuilder}

/** A template for companion objects of <code>Set</code> and subclasses
 *  thereof.
 *
 *  @since 2.8
 */
abstract class SetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
  extends GenericCompanion[CC] {

  def newBuilder[A]: Builder[A, CC[A]] = new AddingBuilder[A, CC[A]](empty[A])

  def setCanBuildFrom[A] = new CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = newBuilder[A]
    def apply() = newBuilder[A]
  }
}
