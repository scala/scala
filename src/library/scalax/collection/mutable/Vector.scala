/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Vector.scala 15437 2008-06-25 16:22:45Z stepancheg $

package scalax.collection.mutable

import generic._

trait Vector[A] extends Sequence[A] with collection.Vector[A] with generic.mutable.VectorTemplate[Vector, A]

/* Factory object for `Vector` class */
object Vector extends SequenceFactory[Vector] {
  /** The empty vector */
  def apply[A](args: A*): Vector[A] = ArrayBuffer.apply(args: _*) // !!! swicth to Array?
}
