/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.generic

import Math.MAX_INT
import TraversibleView.NoBuilder

/** A base class for views of Traversible.
 *  Every subclass has to implenment the foreach method
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversibleView[+A, +Coll <: Traversible[_]] extends TraversibleViewTemplate[A, Coll, TraversibleView[A, Coll]]

object TraversibleView {
  class NoBuilder[A] extends Builder[A, Nothing, TraversibleView[_, _]] {
    def +=(elem: A) {}
    def elements: Iterator[A] = Iterator.empty
    def result() = throw new UnsupportedOperationException("TraversibleView.Builder.result")
    def clear() {}
  }
  type Coll = TraversibleView[_, _]
  implicit def builderFactory[A]: BuilderFactory[A, TraversibleView[A, Traversible[_]], Coll] = new BuilderFactory[A, TraversibleView[A, Traversible[_]], Coll] { def apply(from: Coll) = new NoBuilder }
}
