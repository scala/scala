/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import TraversableView.NoBuilder

/** A base class for views of Iterables.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait IterableView[+A, +Coll] extends IterableViewLike[A, Coll, IterableView[A, Coll]]

object IterableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] =
    new CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
