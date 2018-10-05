/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
