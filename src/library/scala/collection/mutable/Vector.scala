/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A subtrait of <code>collection.Vector</code> which represents sequences
 *  that can be mutated.
 */
trait Vector[A] extends Seq[A]
                   with scala.collection.Vector[A]
                   with GenericTraversableTemplate[A, Vector]
                   with VectorLike[A, Vector[A]] {
  override def companion: GenericCompanion[Vector]  = Vector
}

object Vector extends SeqFactory[Vector] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Vector[A]] =
    new GenericCanBuildFrom[A] {
      def apply() = newBuilder[A]
    }
  def newBuilder[A]: Builder[A, Vector[A]] = new ArrayBuffer
}
