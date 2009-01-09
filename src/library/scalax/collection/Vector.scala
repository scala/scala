/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Vector.scala 15437 2008-06-25 16:22:45Z stepancheg $

package scalax.collection

import generic._
import mutable.ArrayBuffer

trait Vector[+A] extends Sequence[A] with covariant.VectorTemplate[Vector, A]

object Vector extends covariant.SequenceFactory[Vector] {

  /** The empty sequence */
  val empty : Vector[Nothing] = null // !!! todo: insert good immutable vector implementation here.
}
