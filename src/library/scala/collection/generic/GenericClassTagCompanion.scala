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
import reflect.ClassTag

/** This class represents companions of classes which require ClassTags
 *  for their element types.
 *
 *  @author Aleksandar Prokopec
 */
abstract class GenericClassTagCompanion[+CC[X] <: Traversable[X]] {
  type Coll = CC[_]

  def newBuilder[A](implicit ord: ClassTag[A]): Builder[A, CC[A]]

  def empty[A: ClassTag]: CC[A] = newBuilder[A].result

  def apply[A](elems: A*)(implicit ord: ClassTag[A]): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result
  }
}
