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

/** A type for which there is always an implicit value. */
class DummyImplicit

object DummyImplicit {
  /** An implicit value yielding a `DummyImplicit`. */
  implicit def dummyImplicit: DummyImplicit = new DummyImplicit
}
