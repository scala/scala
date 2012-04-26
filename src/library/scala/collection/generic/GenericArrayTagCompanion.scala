/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import mutable.Builder
import language.higherKinds

/** This class represents companions of classes which require ArrayTags
 *  for their element types.
 *
 *  @author Aleksandar Prokopec
 */
abstract class GenericArrayTagCompanion[+CC[X] <: Traversable[X]] {
  type Coll = CC[_]

  def newBuilder[A](implicit ord: ArrayTag[A]): Builder[A, CC[A]]

  def empty[A: ArrayTag]: CC[A] = newBuilder[A].result

  def apply[A](elems: A*)(implicit ord: ArrayTag[A]): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result
  }
}
