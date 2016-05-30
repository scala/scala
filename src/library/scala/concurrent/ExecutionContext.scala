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
   *
   *  @param runnable  the task to execute
   */
  def execute(runnable: Runnable): Unit

  /** Reports that an asynchronous computation failed.
   *
   *  @param cause  the cause of the failure
   */
  def reportFailure(@deprecatedName('t) cause: Throwable): Unit

  /** Prepares for the execution of a task. Returns the prepared
     *  execution context. The recommended implementation of
     *  `prepare` is to return `this`.
     *
     *  This method should no longer be overridden or called. It was
     *  originally expected that `prepare` would be called by
     *  all libraries that consume ExecutionContexts, in order to
     *  capture thread local context. However, this usage has proven
     *  difficult to implement in practice and instead it is
     *  now better to avoid using `prepare` entirely.
     *
     *  Instead, if an `ExecutionContext` needs to capture thread
     *  local context, it should capture that context when it is
     *  constructed, so that it doesn't need any additional
     *  preparation later.
     */
  @deprecated("preparation of ExecutionContexts will be removed", "2.12.0")
  def prepare(): ExecutionContext = this
}

/**
 * An [[ExecutionContext]] that is also a
 * Java [[http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executor.html Executor]].
 */
trait ExecutionContextExecutor extends ExecutionContext with Executor

/**
 * An [[ExecutionContext]] that is also a
 * Java [[http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html ExecutorService]].
 */
trait ExecutionContextExecutorService extends ExecutionContextExecutor with ExecutorService


/** Contains factory methods for creating execution contexts.
 */
object ExecutionContext {
  /**
   * The explicit global `ExecutionContext`. Invoke `global` when you want to provide the global
   * `ExecutionContext` explicitly.
   *
   * The default `ExecutionContext` implementation is backed by a work-stealing thread pool. By default,
   * the thread pool uses a target number of worker threads equal to the number of
   * [[https://docs.oracle.com/javase/8/docs/api/java/lang/Runtime.html#availableProcessors-- available processors]].
   *
   * @return the global `ExecutionContext`
   */
  def global: ExecutionContextExecutor = Implicits.global.asInstanceOf[ExecutionContextExecutor]

  object Implicits {
    /**
     * The implicit global `ExecutionContext`. Import `global` when you want to provide the global
     * `ExecutionContext` implicitly.
     *
     * The default `ExecutionContext` implementation is backed by a work-stealing thread pool. By default,
     * the thread pool uses a target number of worker threads equal to the number of
     * [[https://docs.oracle.com/javase/8/docs/api/java/lang/Runtime.html#availableProcessors-- available processors]].
     */
    implicit lazy val global: ExecutionContext = impl.ExecutionContextImpl.fromExecutor(null: Executor)
  }

  /** Creates an `ExecutionContext` from the given `ExecutorService`.
   *
   *  @param e         the `ExecutorService` to use. If `null`, a new `ExecutorService` is created with [[http://www.scala-lang.org/api/current/index.html#scala.concurrent.ExecutionContext$@global:scala.concurrent.ExecutionContextExecutor default configuration]].
   *  @param reporter  a function for error reporting
   *  @return          the `ExecutionContext` using the given `ExecutorService`
   */
  def fromExecutorService(e: ExecutorService, reporter: Throwable => Unit): ExecutionContextExecutorService =
    impl.ExecutionContextImpl.fromExecutorService(e, reporter)

  /** Creates an `ExecutionContext` from the given `ExecutorService` with the [[scala.concurrent.ExecutionContext$.defaultReporter default reporter]].
   *
   *  If it is guaranteed that none of the executed tasks are blocking, a single-threaded `ExecutorService`
   *  can be used to create an `ExecutionContext` as follows:
   *
   *  {{{
   *  import java.util.concurrent.Executors
   *  val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
   *  }}}
   *
   *  @param e the `ExecutorService` to use. If `null`, a new `ExecutorService` is created with [[http://www.scala-lang.org/api/current/index.html#scala.concurrent.ExecutionContext$@global:scala.concurrent.ExecutionContextExecutor default configuration]].
   *  @return  the `ExecutionContext` using the given `ExecutorService`
   */
  def fromExecutorService(e: ExecutorService): ExecutionContextExecutorService = fromExecutorService(e, defaultReporter)

  /** Creates an `ExecutionContext` from the given `Executor`.
   *
   *  @param e         the `Executor` to use. If `null`, a new `Executor` is created with [[http://www.scala-lang.org/api/current/index.html#scala.concurrent.ExecutionContext$@global:scala.concurrent.ExecutionContextExecutor default configuration]].
   *  @param reporter  a function for error reporting
   *  @return          the `ExecutionContext` using the given `Executor`
   */
  def fromExecutor(e: Executor, reporter: Throwable => Unit): ExecutionContextExecutor =
    impl.ExecutionContextImpl.fromExecutor(e, reporter)

  /** Creates an `ExecutionContext` from the given `Executor` with the [[scala.concurrent.ExecutionContext$.defaultReporter default reporter]].
   *
   *  @param e the `Executor` to use. If `null`, a new `Executor` is created with [[http://www.scala-lang.org/api/current/index.html#scala.concurrent.ExecutionContext$@global:scala.concurrent.ExecutionContextExecutor default configuration]].
   *  @return  the `ExecutionContext` using the given `Executor`
   */
  def fromExecutor(e: Executor): ExecutionContextExecutor = fromExecutor(e, defaultReporter)

  /** The default reporter simply prints the stack trace of the `Throwable` to [[http://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err System.err]].
   *
   *  @return the function for error reporting
   */
  def defaultReporter: Throwable => Unit = _.printStackTrace()
}


