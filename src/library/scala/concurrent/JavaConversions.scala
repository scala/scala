/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
