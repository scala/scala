/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

/** A template for companion objects of <code>Set</code> and subclasses
 *  thereof.
 */
abstract class SetFactory[CC[X] <: Set[X] with SetTemplate[X, CC[X]]]
  extends Companion[CC] {

  def newBuilder[A]: Builder[A, CC[A]] = new AddingBuilder[A, CC[A]](empty[A])

  def setBuilderFactory[A] = new BuilderFactory[A, CC[A], CC[_]] {
    def apply(from: CC[_]) = newBuilder[A]
  }
}
