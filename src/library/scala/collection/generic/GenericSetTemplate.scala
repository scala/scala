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
import scala.language.higherKinds
/**
 * @since 2.8
 */
trait GenericSetTemplate[A, +CC[X] <: GenSet[X]] extends GenericTraversableTemplate[A, CC] {
  def empty: CC[A] = companion.empty[A]
}

