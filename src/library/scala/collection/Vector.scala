/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Vector.scala 15437 2008-06-25 16:22:45Z stepancheg $

package scala.collection

import generic._
import mutable.ArrayBuffer

/** Sequences that support O(1) element access and O(1) length computation.
 *  This class does not add any methods to Sequence but overrides several
 *  methods with optimized implementations.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Vector[+A] extends Sequence[A] with VectorTemplate[A, Vector[A]] {
  override protected[this] def newBuilder = Vector.newBuilder
  override def traversableBuilder[B]: Builder[B, Vector[B], Any] = Vector.newBuilder[B]
}

object Vector extends SequenceFactory[Vector] {
  type Coll = Vector[_]
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new BuilderFactory[A, Vector[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Vector[A], Any] = mutable.Vector.newBuilder[A]
}
