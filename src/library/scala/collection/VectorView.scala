/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import TraversableView.NoBuilder
import generic._

/** A non-strict projection of an iterable.
 *
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 * @since   2.8
 */
trait VectorView[+A, +Coll] extends VectorViewLike[A, Coll, VectorView[A, Coll]]

object VectorView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, VectorView[A, Vector[_]]] =
    new CanBuildFrom[Coll, A, VectorView[A, Vector[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
  implicit def arrCanBuildFrom[A]: CanBuildFrom[TraversableView[_, Array[_]], A, VectorView[A, Array[A]]] =
    new CanBuildFrom[TraversableView[_, Array[_]], A, VectorView[A, Array[A]]] {
      def apply(from: TraversableView[_, Array[_]]) = new NoBuilder
      def apply() = new NoBuilder
    }
}
