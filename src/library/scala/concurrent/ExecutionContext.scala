/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent


import java.util.concurrent.{ ExecutorService, Executor }
import scala.annotation.implicitNotFound
import scala.util.Try

/**
 * An `ExecutionContext` can execute program logic asynchronously,
 * typically but not necessarily on a thread pool.
 *
 * A general purpose `ExecutionContext` must be asynchronous in executing
 * any `Runnable` that is passed into its `execute`-method. A special purpose
 * `ExecutionContext` may be synchronous but must only be passed to code that
 * is explicitly safe to be run using a synchronously executing `ExecutionContext`.
 *
 * APIs such as `Future.onComplete` require you to provide a callback
 * and an implicit `ExecutionContext`. The implicit `ExecutionContext`
 * will be used to execute the callback.
 *
 * It is possible to simply import
 * `scala.concurrent.ExecutionContext.Implicits.global` to obtain an
 * implicit `ExecutionContext`. This global context is a reasonable
 * default thread pool.
 *
 * However, application developers should carefully consider where they
 * want to set policy; ideally, one place per application (or per
 * logically-related section of code) will make a decision about
 * which `ExecutionContext` to use. That is, you might want to avoid
 * hardcoding `scala.concurrent.ExecutionContext.Implicits.global` all
 * over the place in your code.
 * One approach is to add `(implicit ec: ExecutionContext)`
 * to methods which need an `ExecutionContext`. Then import a specific
 * context in one place for the entire application or module,
 * passing it implicitly to individual methods.
 *
 * A custom `ExecutionContext` may be appropriate to execute code
 * which blocks on IO or performs long-running computations.
 * `ExecutionContext.fromExecutorService` and `ExecutionContext.fromExecutor`
 * are good ways to create a custom `ExecutionContext`.
 *
 * The intent of `ExecutionContext` is to lexically scope code execution.
 * That is, each method, class, file, package, or application determines
 * how to run its own code. This avoids issues such as running
 * application callbacks on a thread pool belonging to a networking library.
 * The size of a networking library's thread pool can be safely configured,
 * knowing that only that library's network operations will be affected.
 * Application callback execution can be configured separately.
 */
@implicitNotFound("""Cannot find an implicit ExecutionContext. You might pass
an (implicit ec: ExecutionContext) parameter to your method
or import scala.concurrent.ExecutionContext.Implicits.global.""")
trait ExecutionContext {

  /** Runs a block of code on this execution context.
   */
  def execute(runnable: Runnable): Unit

  /** Reports that an asynchronous computation failed.
   */
  def reportFailure(@deprecatedName('t) cause: Throwable): Unit

  /** Prepares for the execution of a task. Returns the prepared
   *  execution context. A valid implementation of `prepare` is one
   *  that simply returns `this`.
   */
  def prepare(): ExecutionContext = this

}

/**
 * An [[ExecutionContext]] that is also a Java [[Executor]].
 */
trait ExecutionContextExecutor extends ExecutionContext with Executor

/**
 * An [[ExecutionContext]] that is also a Java [[ExecutorService]].
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
  def defaultReporter: Throwable => Unit = _.printStackTrace()
}


