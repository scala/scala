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
trait Vector[+A] extends Sequence[A]
                    with TraversableClass[A, Vector]
                    with VectorTemplate[A, Vector[A]] {
  override def companion: Companion[Vector] = Vector
}

object Vector extends SequenceFactory[Vector] {
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Vector[A]] = mutable.Vector.newBuilder[A]
}
