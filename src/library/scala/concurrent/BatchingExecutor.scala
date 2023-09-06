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

import java.util.ArrayDeque
import java.util.concurrent.Executor

/**
 * Mixin trait for an Executor
 * which groups multiple nested `Runnable.run()` calls
 * into a single Runnable passed to the original
 * Executor. This can be a useful optimization
 * because it bypasses the original context's task
 * queue and keeps related (nested) code on a single
 * thread which may improve CPU affinity. However,
 * if tasks passed to the Executor are blocking
 * or expensive, this optimization can prevent work-stealing
 * and make performance worse. Also, some ExecutionContext
 * may be fast enough natively that this optimization just
 * adds overhead.
 * The default ExecutionContext.global is already batching
 * or fast enough not to benefit from it; while
 * `fromExecutor` and `fromExecutorService` do NOT add
 * this optimization since they don't know whether the underlying
 * executor will benefit from it.
 * A batching executor can create deadlocks if code does
 * not use `scala.concurrent.blocking` when it should,
 * because tasks created within other tasks will block
 * on the outer task completing.
 * This executor may run tasks in any order, including LIFO order.
 * There are no ordering guarantees.
 *
 * WARNING: The underlying Executor's execute-method must not execute the submitted Runnable
 * in the calling thread synchronously. It must enqueue/handoff the Runnable.
 */
private[concurrent] trait BatchingExecutor extends Executor {

  // null if not yet running a batch
  private val currentBatch = new ThreadLocal[Batch]()

  // null if not yet in a block context
  private val currentBlockContext = new ThreadLocal[BlockContext]()

  private class Batch(isBlockable: Boolean = false) extends Runnable with BlockContext {

    private val tasks: ArrayDeque[Runnable] = new ArrayDeque()

    def add(r: Runnable): this.type = {
      tasks.addFirst(r)
      this
    }

    /** If the batch is us, drain the tasks queue. The given batch is the current batch, if any. */
    def processBatch(batch: Batch): Unit =
      if (batch.eq(this) && !tasks.isEmpty) {
        tasks.poll().run()
        processBatch(currentBatch.get)
      }

    // this method runs in the delegate ExecutionContext's thread
    def run(): Unit = {
      require(currentBatch.get eq null)

      currentBatch.set(this) // we are up

      def process() =
        try processBatch(this)
        catch { case t: Throwable => resubmitUnbatched(); throw t }
        finally currentBatch.remove()

      if (isBlockable) {
        val setContext = currentBlockContext.get.eq(null) && {
          currentBlockContext.set(BlockContext.current)
          true
        }
        BlockContext.withBlockContext(this) {
          try process()
          finally if (setContext) currentBlockContext.remove()
        }
      }
      else process()
    }

    def resubmitUnbatched(): Unit = {
      val batch = currentBatch.get
      currentBatch.remove()
      if (batch.eq(this) && !tasks.isEmpty)
        unbatchedExecute(batch)
    }

    def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      // if we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
      resubmitUnbatched()

      // now delegate the blocking to the previous BC
      currentBlockContext.get.blockOn(thunk)
    }
  }

  /** Execute directly. */
  protected def unbatchedExecute(r: Runnable): Unit

  /** Whether blocking is managed. */
  protected def resubmitOnBlock: Boolean

  /** If the given runnable is batchable, run in batch mode; otherwise, execute it directly. */
  override def execute(runnable: Runnable): Unit =
    if (batchable(runnable)) {
      currentBatch.get match {
        // if we aren't in batching mode yet, enqueue batch
        case null => unbatchedExecute(new Batch(isBlockable = resubmitOnBlock).add(runnable))
        // if we are already in batching mode, add to batch
        case some => some.add(runnable)
      }
    }
    else unbatchedExecute(runnable)

  /** Override this to define which runnables will be batched. */
  def batchable(runnable: Runnable): Boolean = runnable match {
    case _: OnCompleteRunnable => true
    case _                     => false
  }
}
