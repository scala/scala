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

package scala.concurrent

import java.util.concurrent.{ExecutorService, Executor}
import scala.language.implicitConversions

/** The `JavaConversions` object provides implicit conversions supporting
 *  interoperability between Scala and Java concurrency classes.
 *
 *  @author Philipp Haller
 */
object JavaConversions {

  /**
   * Creates a new `ExecutionContext` which uses the provided `ExecutorService`.
   */
  implicit def asExecutionContext(exec: ExecutorService): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(exec)

  /**
   * Creates a new `ExecutionContext` which uses the provided `Executor`.
   */
  implicit def asExecutionContext(exec: Executor): ExecutionContextExecutor =
    ExecutionContext.fromExecutor(exec)

}
