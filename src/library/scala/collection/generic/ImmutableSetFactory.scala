/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import mutable.{ Builder, SetBuilder }
import scala.language.higherKinds

abstract class ImmutableSetFactory[CC[X] <: immutable.Set[X] with SetLike[X, CC[X]]]
  extends SetFactory[CC] {
  private[collection] def emptyInstance: CC[Any]
  override def empty[A] = emptyInstance.asInstanceOf[CC[A]]
  def newBuilder[A]: Builder[A, CC[A]] = new SetBuilder[A, CC[A]](empty[A])
}
