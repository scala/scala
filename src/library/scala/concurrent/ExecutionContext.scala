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
 * While it is possible to simply import
 * `scala.concurrent.ExecutionContext.Implicits.global` to obtain an
 * implicit `ExecutionContext`, application developers should carefully
 * consider where they want to set execution policy;
 * ideally, one place per application—or per logically related section of code—
 * will make a decision about which `ExecutionContext` to use.
 * That is, you will mostly want to avoid hardcoding, especially via an import,
 * `scala.concurrent.ExecutionContext.Implicits.global`.
 * The recommended approach is to add `(implicit ec: ExecutionContext)` to methods,
 * or class constructor parameters, which need an `ExecutionContext`.
 * 
 * Then locally import a specific `ExecutionContext` in one place for the entire
 * application or module, passing it implicitly to individual methods.
 * Alternatively define a local implicit val with the required `ExecutionContext`.
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
an (implicit ec: ExecutionContext) parameter to your method.

The ExecutionContext is used to configure how and on which
thread pools Futures will run, so the specific ExecutionContext
that is selected is important.

If your application does not define an ExecutionContext elsewhere,
consider using Scala's global ExecutionContext by defining
the following:

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global""")
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
  def reportFailure(@deprecatedName("t") cause: Throwable): Unit

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
  // This cannot be removed until there is a suitable replacement
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
   * The default `ExecutionContext` implementation is backed by a work-stealing thread pool.
   * It can be configured via the following [[scala.sys.SystemProperties]]:
   *
   * `scala.concurrent.context.minThreads` = defaults to "1"
   * `scala.concurrent.context.numThreads` = defaults to "x1" (i.e. the current number of available processors * 1)
   * `scala.concurrent.context.maxThreads` = defaults to "x1" (i.e. the current number of available processors * 1)
   * `scala.concurrent.context.maxExtraThreads` = defaults to "256"
   *
   * The pool size of threads is then `numThreads` bounded by `minThreads` on the lower end and `maxThreads` on the high end.
   *
   * The `maxExtraThreads` is the maximum number of extra threads to have at any given time to evade deadlock,
   * see [[scala.concurrent.BlockContext]].
   *
   * @return the global `ExecutionContext`
   */
  final lazy val global: ExecutionContextExecutor = impl.ExecutionContextImpl.fromExecutor(null: Executor)

  /**
   * This `ExecutionContext` is ideally suited to execute short-lived tasks on the `global` `ExecutionContext` as it
   * attempts to locally batch the execution of nested tasks.
   *
   * WARNING: long-running and/or blocking tasks should be demarcated within `scala.concurrent.blocking`-blocks,
   *          to ensure that any pending tasks in the current batch can be executed by another thread on `global`.
   */
  object batchingGlobal extends ExecutionContextExecutor with BatchingExecutor {
    final override def submitForExecution(runnable: Runnable): Unit = global.execute(runnable)

    final override def execute(runnable: Runnable): Unit =
      if ((!runnable.isInstanceOf[impl.Promise.Transformation[_,_]] || runnable.asInstanceOf[impl.Promise.Transformation[_,_]].benefitsFromBatching) && runnable.isInstanceOf[Batchable])
        submitAsyncBatched(runnable)
      else
        submitForExecution(runnable)

    override final def reportFailure(t: Throwable): Unit = global.reportFailure(t)
  }

  /**
   * WARNING: Only ever execute logic which will quickly return control to the caller.
   *
   * This `ExecutionContext` steals execution time from other threads by having its
   * `Runnable`s run on the `Thread` which calls `execute` and then yielding back control
   * to the caller after *all* its `Runnable`s have been executed.
   * Nested invocations of `execute` will be trampolined to prevent uncontrolled stack space growth.
   *
   * When using `parasitic` with abstractions such as `Future` it will in many cases be non-deterministic
   * as to which `Thread` will be executing the logic, as it depends on when/if that `Future` is completed.
   *
   * Do *not* call any blocking code in the `Runnable`s submitted to this `ExecutionContext`
   * as it will prevent progress by other enqueued `Runnable`s and the calling `Thread`.
   * 
   * Symptoms of misuse of this `ExecutionContext` include, but are not limited to, deadlocks
   * and severe performance problems.
   *
   * Any `NonFatal` or `InterruptedException`s will be reported to the `defaultReporter`.
   */
  object parasitic extends ExecutionContextExecutor with BatchingExecutor {
    override final def submitForExecution(runnable: Runnable): Unit = runnable.run()
    override final def execute(runnable: Runnable): Unit = submitSyncBatched(runnable)
    override final def reportFailure(t: Throwable): Unit = defaultReporter(t)
  }

  object Implicits {
    /**
     * The implicit global `ExecutionContext`. Import `global` when you want to provide the global
     * `ExecutionContext` implicitly.
     *
     * The default `ExecutionContext` implementation is backed by a work-stealing thread pool. By default,
     * the thread pool uses a target number of worker threads equal to the number of
     * [[https://docs.oracle.com/javase/8/docs/api/java/lang/Runtime.html#availableProcessors-- available processors]].
     */
    implicit final def global: ExecutionContext = ExecutionContext.global
  }

  /** Creates an `ExecutionContext` from the given `ExecutorService`.
   *
   *  @param e         the `ExecutorService` to use. If `null`, a new `ExecutorService` is created with [[scala.concurrent.ExecutionContext$.global default configuration]].
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
   *  @param e the `ExecutorService` to use. If `null`, a new `ExecutorService` is created with [[scala.concurrent.ExecutionContext$.global default configuration]].
   *  @return  the `ExecutionContext` using the given `ExecutorService`
   */
  def fromExecutorService(e: ExecutorService): ExecutionContextExecutorService = fromExecutorService(e, defaultReporter)

  /** Creates an `ExecutionContext` from the given `Executor`.
   *
   *  @param e         the `Executor` to use. If `null`, a new `Executor` is created with [[scala.concurrent.ExecutionContext$.global default configuration]].
   *  @param reporter  a function for error reporting
   *  @return          the `ExecutionContext` using the given `Executor`
   */
  def fromExecutor(e: Executor, reporter: Throwable => Unit): ExecutionContextExecutor =
    impl.ExecutionContextImpl.fromExecutor(e, reporter)

  /** Creates an `ExecutionContext` from the given `Executor` with the [[scala.concurrent.ExecutionContext$.defaultReporter default reporter]].
   *
   *  @param e the `Executor` to use. If `null`, a new `Executor` is created with [[scala.concurrent.ExecutionContext$.global default configuration]].
   *  @return  the `ExecutionContext` using the given `Executor`
   */
  def fromExecutor(e: Executor): ExecutionContextExecutor = fromExecutor(e, defaultReporter)

  /** The default reporter simply prints the stack trace of the `Throwable` to [[http://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err System.err]].
   *
   *  @return the function for error reporting
   */
  final val defaultReporter: Throwable => Unit = _.printStackTrace()
}
