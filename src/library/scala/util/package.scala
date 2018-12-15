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

  @deprecated("Use `Either.Left` instead.", since = "2.13.0")
  type Left[+A, +B] = scala.util.Either.Left[A, B]
  @deprecated("Use `Either.Left` instead.", since = "2.13.0")
  val Left: Either.Left.type = scala.util.Either.Left

  @deprecated("Use `Either.Right` instead.", since = "2.13.0")
  type Right[+A, +B] = scala.util.Either.Right[A, B]
  @deprecated("Use `Either.Right` instead.", since = "2.13.0")
  val Right: Either.Right.type = scala.util.Either.Right
}