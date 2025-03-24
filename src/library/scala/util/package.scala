/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

package object util {
  /**
   * Adds chaining methods `tap` and `pipe` to every type. See [[ChainingOps]].
   */
  object chaining extends ChainingSyntax
}
