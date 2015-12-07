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
import TraversableView.NoBuilder

/** A base trait for non-strict views of `Iterable`s.
 *  $iterableViewInfo
 */
trait IterableView[+A, +Coll] extends IterableViewLike[A, Coll, IterableView[A, Coll]]

/** An object containing the necessary implicit definitions to make
 *  `IterableView`s work. Its definitions are generally not accessed directly by clients.
 */
object IterableView {
  type Coll = TraversableView[_, _ <: Traversable[_]]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] =
    new CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
