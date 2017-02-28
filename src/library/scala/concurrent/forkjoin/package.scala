/*                     __                                                 *\
**     ________ ___   / /  ___     Scala API                              **
**    / __/ __// _ | / /  / _ |    (c) 2015, LAMP/EPFL and Typesafe, Inc. **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/                 **
** /____/\___/_/ |_/____/_/ | |                                           **
**                          |/                                            **
\*                                                                        */

package scala.concurrent
import java.util.{concurrent => juc}
import java.util.Collection

package object forkjoin {
  @deprecated("use java.util.concurrent.ForkJoinPool directly, instead of this alias",         "2.12.0")
  type ForkJoinPool = juc.ForkJoinPool
  @deprecated("use java.util.concurrent.ForkJoinPool directly, instead of this alias",         "2.12.0")
  object ForkJoinPool {
    type ForkJoinWorkerThreadFactory = juc.ForkJoinPool.ForkJoinWorkerThreadFactory
    type ManagedBlocker              = juc.ForkJoinPool.ManagedBlocker

    val defaultForkJoinWorkerThreadFactory: ForkJoinWorkerThreadFactory = juc.ForkJoinPool.defaultForkJoinWorkerThreadFactory
    def managedBlock(blocker: ManagedBlocker): Unit                     = juc.ForkJoinPool.managedBlock(blocker)
  }

  @deprecated("use java.util.concurrent.ForkJoinTask directly, instead of this alias",         "2.12.0")
  type ForkJoinTask[T] = juc.ForkJoinTask[T]
  @deprecated("use java.util.concurrent.ForkJoinTask directly, instead of this alias",         "2.12.0")
  object ForkJoinTask extends scala.Serializable {
    def adapt(runnable: Runnable): ForkJoinTask[_]                           = juc.ForkJoinTask.adapt(runnable)
    def adapt[T](callable: juc.Callable[_ <: T]): ForkJoinTask[T]            = juc.ForkJoinTask.adapt(callable)
    def adapt[T](runnable: Runnable, result: T): ForkJoinTask[T]             = juc.ForkJoinTask.adapt(runnable, result)
    def getPool(): ForkJoinPool                                              = juc.ForkJoinTask.getPool
    def getQueuedTaskCount(): Int                                            = juc.ForkJoinTask.getQueuedTaskCount
    def getSurplusQueuedTaskCount(): Int                                     = juc.ForkJoinTask.getSurplusQueuedTaskCount
    def helpQuiesce(): Unit                                                  = juc.ForkJoinTask.helpQuiesce
    def inForkJoinPool(): Boolean                                            = juc.ForkJoinTask.inForkJoinPool
    def invokeAll[T <: ForkJoinTask[_]](tasks: Collection[T]): Collection[T] = juc.ForkJoinTask.invokeAll(tasks)
    def invokeAll[T](t1: ForkJoinTask[T]): Unit                              = juc.ForkJoinTask.invokeAll(t1)
    def invokeAll[T](tasks: ForkJoinTask[T]*): Unit                          = juc.ForkJoinTask.invokeAll(tasks: _*)
  }

  @deprecated("use java.util.concurrent.ForkJoinWorkerThread directly, instead of this alias", "2.12.0")
  type ForkJoinWorkerThread   = juc.ForkJoinWorkerThread
  @deprecated("use java.util.concurrent.LinkedTransferQueue directly, instead of this alias",  "2.12.0")
  type LinkedTransferQueue[T] = juc.LinkedTransferQueue[T]
  @deprecated("use java.util.concurrent.RecursiveAction directly, instead of this alias",      "2.12.0")
  type RecursiveAction        = juc.RecursiveAction
  @deprecated("use java.util.concurrent.RecursiveTask directly, instead of this alias",        "2.12.0")
  type RecursiveTask[T]       = juc.RecursiveTask[T]

  @deprecated("use java.util.concurrent.ThreadLocalRandom directly, instead of this alias",    "2.12.0")
  type ThreadLocalRandom      = juc.ThreadLocalRandom
  @deprecated("use java.util.concurrent.ThreadLocalRandom directly, instead of this alias",    "2.12.0")
  object ThreadLocalRandom extends scala.Serializable {
    // For source compatibility, current must declare the empty argument list.
    // Having no argument list makes more sense since it doesn't have any side effects,
    // but existing callers will break if they invoked it as `current()`.
    def current() = juc.ThreadLocalRandom.current
  }
}
