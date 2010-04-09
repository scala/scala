/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.Builder
import TraversableView.NoBuilder

/** $traversableviewinfo
 */
trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]]

object TraversableView {
  class NoBuilder[A] extends Builder[A, Nothing] {
    def +=(elem: A): this.type = this
    def iterator: Iterator[A] = Iterator.empty
    @deprecated("use `iterator' instead") def elements = iterator
    def result() = throw new UnsupportedOperationException("TraversableView.Builder.result")
    def clear() {}
  }
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] =
    new CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
