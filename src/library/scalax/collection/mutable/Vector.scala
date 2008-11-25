/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Vector.scala 15437 2008-06-25 16:22:45Z stepancheg $

package scalax.collection.mutable

trait Vector[A] extends collection.Vector[A] with generic.mutable.VectorTemplate[Vector, A]

object Vector extends generic.nonvariant.SequenceFactory[Vector] {

  /** The empty iterable */
  def apply[A](args: A*): Vector[A] = null // !!!

}
