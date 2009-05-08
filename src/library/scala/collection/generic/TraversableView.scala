/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.generic

import Math.MAX_INT
import TraversableView.NoBuilder

/** A base class for views of Traversable.
 *  Every subclass has to implenment the foreach method
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableView[+A, +Coll <: Traversable[_]] extends TraversableViewTemplate[A, Coll, TraversableView[A, Coll]]

object TraversableView {
  class NoBuilder[A] extends Builder[A, Nothing, TraversableView[_, _]] {
    def +=(elem: A) {}
    def elements: Iterator[A] = Iterator.empty
    def result() = throw new UnsupportedOperationException("TraversableView.Builder.result")
    def clear() {}
  }
  type Coll = TraversableView[_, _]
  implicit def builderFactory[A]: BuilderFactory[A, TraversableView[A, Traversable[_]], Coll] = new BuilderFactory[A, TraversableView[A, Traversable[_]], Coll] { def apply(from: Coll) = new NoBuilder }
}
