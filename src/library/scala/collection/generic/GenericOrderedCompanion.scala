/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import mutable.Builder
import scala.language.higherKinds

/** This class represents companions of classes which require the ordered trait
 *  for their element types.
 *
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
abstract class GenericOrderedCompanion[+CC[X] <: Traversable[X]] {
  protected[this] type Coll = CC[_]

  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]]

  def empty[A: Ordering]: CC[A] = newBuilder[A].result()

  def apply[A](elems: A*)(implicit ord: Ordering[A]): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result()
  }
}

