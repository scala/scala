/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package generic

import mutable.Builder





abstract class GenericOrderedCompanion[+CC[X] <: Traversable[X]] {
  type Coll = CC[_]

  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]]

  def empty[A: Ordering]: CC[A] = newBuilder[A].result

  def apply[A](elems: A*)(implicit ord: Ordering[A]): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result
  }
}

