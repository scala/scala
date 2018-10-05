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

import mutable.{ Builder, GrowingBuilder }
import scala.language.higherKinds

abstract class MutableSetFactory[CC[X] <: mutable.Set[X] with mutable.SetLike[X, CC[X]]]
  extends SetFactory[CC] {

  def newBuilder[A]: Builder[A, CC[A]] = new GrowingBuilder[A, CC[A]](empty[A])
}
