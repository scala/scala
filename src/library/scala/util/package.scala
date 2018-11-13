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

package object util {
  /**
   * Adds chaining methods `tap` and `pipe` to every type. See [[ChainingOps]].
   */
  object chaining extends ChainingSyntax

  @deprecated("Use `Try.Failure` instead.", since = "2.13.0")
  type Failure[+T] = scala.util.Try.Failure[T]
  @deprecated("Use `Try.Failure` instead.", since = "2.13.0")
  val Failure: Try.Failure.type = scala.util.Try.Failure

  @deprecated("Use `Try.Success` instead.", since = "2.13.0")
  type Success[+T] = scala.util.Try.Success[T]
  @deprecated("Use `Try.Success` instead.", since = "2.13.0")
  val Success: Try.Success.type = scala.util.Try.Success
}