/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import mutable.Builder

/** A base trait for non-strict views of traversable collections.
 *  $traversableViewInfo
 */
trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]] { }

/** An object containing the necessary implicit definitions to make
 *  `TraversableView`s work. Its definitions are generally not accessed directly by clients.
 */
object TraversableView {
  class NoBuilder[A] extends Builder[A, Nothing] {
    def +=(elem: A): this.type = this
    def iterator: Iterator[A] = Iterator.empty
    def result() = throw new UnsupportedOperationException("TraversableView.Builder.result")
    def clear() {}
  }
  type Coll = TraversableView[_, _ <: Traversable[_]]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] =
    new CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
