/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent


import java.util.concurrent.{ ExecutorService, Executor }
import scala.concurrent.util.Duration
import scala.annotation.implicitNotFound
import scala.util.Try

/**
 * An `ExecutionContext` is an abstraction over an entity that can execute program logic.
 */
@implicitNotFound("Cannot find an implicit ExecutionContext, either require one yourself or import ExecutionContext.Implicits.global")
trait ExecutionContext {
  
  /** Runs a block of code on this execution context.
   */
  def execute(runnable: Runnable): Unit
  
  /** Reports that an asynchronous computation failed.
   */
  def reportFailure(t: Throwable): Unit
  
  /** Prepares for the execution of a task. Returns the prepared
   *  execution context. A valid implementation of `prepare` is one
   *  that simply returns `this`.
   */
  def prepare(): ExecutionContext = this

}

/**
 * Union interface since Java does not support union types
 */
trait ExecutionContextExecutor extends ExecutionContext with Executor

/**
 * Union interface since Java does not support union types
 */
trait ExecutionContextExecutorService extends ExecutionContextExecutor with ExecutorService


/** Contains factory methods for creating execution contexts.
 */
object ExecutionContext {
  /**
   * This is the explicit global ExecutionContext,
   * call this when you want to provide the global ExecutionContext explicitly
   */
  def global: ExecutionContextExecutor = Implicits.global

  object Implicits {
    /**
     * This is the implicit global ExecutionContext,
     * import this when you want to provide the global ExecutionContext implicitly
     */
    implicit lazy val global: ExecutionContextExecutor = impl.ExecutionContextImpl.fromExecutor(null: Executor)
  }
    
  /** Creates an `ExecutionContext` from the given `ExecutorService`.
   */
  def fromExecutorService(e: ExecutorService, reporter: Throwable => Unit): ExecutionContextExecutorService =
    impl.ExecutionContextImpl.fromExecutorService(e, reporter)

  /** Creates an `ExecutionContext` from the given `ExecutorService` with the default Reporter.
   */
  def fromExecutorService(e: ExecutorService): ExecutionContextExecutorService = fromExecutorService(e, defaultReporter)
  
  /** Creates an `ExecutionContext` from the given `Executor`.
   */
  def fromExecutor(e: Executor, reporter: Throwable => Unit): ExecutionContextExecutor =
    impl.ExecutionContextImpl.fromExecutor(e, reporter)

  /** Creates an `ExecutionContext` from the given `Executor` with the default Reporter.
   */
  def fromExecutor(e: Executor): ExecutionContextExecutor = fromExecutor(e, defaultReporter)
  
  /** The default reporter simply prints the stack trace of the `Throwable` to System.err.
   */
  def defaultReporter: Throwable => Unit = (t: Throwable) => t.printStackTrace()
}


