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

package scala.collection.convert

/** An (optional) marker trait that indicates that a `Stepper` can call `substep` with
  * at worst O(log N) time and space complexity, and that the division is likely to
  * be reasonably even.
  */
trait EfficientSubstep
