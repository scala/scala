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

package scala.concurrent

import java.util.concurrent.{ExecutorService, Executor}
import scala.language.implicitConversions

/** The `JavaConversions` object provides implicit conversions supporting
 *  interoperability between Scala and Java concurrency classes.
 */
@deprecated("Use the factory methods in `ExecutionContext` instead", "2.13.0")
object JavaConversions {

  /**
   * Creates a new `ExecutionContext` which uses the provided `ExecutorService`.
   */
  @deprecated("Use `ExecutionContext.fromExecutorService` instead", "2.13.0")
  implicit def asExecutionContext(exec: ExecutorService): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(exec)

  /**
   * Creates a new `ExecutionContext` which uses the provided `Executor`.
   */
  @deprecated("Use `ExecutionContext.fromExecutor` instead", "2.13.0")
  implicit def asExecutionContext(exec: Executor): ExecutionContextExecutor =
    ExecutionContext.fromExecutor(exec)

}
