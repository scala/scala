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

import java.util.concurrent.Executor
import java.util.Objects
import scala.util.control.NonFatal
import scala.annotation.{switch, tailrec}

/**
 * Marker trait to indicate that a Runnable is Batchable by BatchingExecutors
 */
trait Batchable {
  self: Runnable =>
}

private[concurrent] object BatchingExecutorStatics {
  final val emptyBatchArray: Array[Runnable] = new Array[Runnable](0)

  // Max number of Runnables executed nested before starting to batch (to prevent stack exhaustion)
  final val syncPreBatchDepth = 16

  // Max number of Runnables processed in one go (to prevent starvation of other tasks on the pool)
  final val runLimit = 1024

  object MissingParentBlockContext extends BlockContext {
    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T =
      try thunk finally throw new IllegalStateException("BUG in BatchingExecutor.Batch: parentBlockContext is null")
  }
}

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
 * and make performance worse.
 * A batching executor can create deadlocks if code does
 * not use `scala.concurrent.blocking` when it should,
 * because tasks created within other tasks will block
 * on the outer task completing.
 * This executor may run tasks in any order, including LIFO order.
 * There are no ordering guarantees.
 *
 * WARNING: Only use *EITHER* `submitAsyncBatched` OR `submitSyncBatched`!!
 *
 * When you implement this trait for async executors like thread pools,
 * you're going to need to implement it something like the following:
 *
 * {{{
 *  final override def submitAsync(runnable: Runnable): Unit =
 *    super[SuperClass].execute(runnable) // To prevent reentrancy into `execute`
 *
 *  final override def execute(runnable: Runnable): Unit =
 *    if (runnable.isInstanceOf[Batchable]) // Or other logic
 *      submitAsyncBatched(runnable)
 *    else
 *      submitAsync(runnable)
 *
 *  final override def reportFailure(cause: Throwable): Unit = …
 *  }}}
 *
 *  And if you want to implement if for a sync, trampolining, executor you're
 *  going to implement it something like this:
 *
 * {{{
 *  final override def submitAsync(runnable: Runnable): Unit = ()
 *
 *  final override def execute(runnable: Runnable): Unit =
 *    submitSyncBatched(runnable) // You typically will want to batch everything
 *
 *  final override def reportFailure(cause: Throwable): Unit =
 *    ExecutionContext.defaultReporter(cause) // Or choose something more fitting
 * }}}
 *
 */
private[concurrent] trait BatchingExecutor extends Executor {
  private[this] final val _tasksLocal = new ThreadLocal[AnyRef]()

  /*
   * Batch implements a LIFO queue (stack) and is used as a trampolining Runnable.
   * In order to conserve allocations, the first element in the batch is stored "unboxed" in
   * the `first` field. Subsequent Runnables are stored in the array called `other`.
  */
  private[this] sealed abstract class AbstractBatch protected (protected final var first: Runnable, protected final var other: Array[Runnable], protected final var size: Int) {

    private[this] final def ensureCapacity(curSize: Int): Array[Runnable] = {
      val curOther = this.other
      val curLen = curOther.length
      if (curSize <= curLen) curOther
      else {
        val newLen = if (curLen == 0) 4 else curLen << 1

        if (newLen <= curLen) throw new StackOverflowError("Space limit of asynchronous stack reached: " + curLen)
        val newOther = new Array[Runnable](newLen)
        System.arraycopy(curOther, 0, newOther, 0, curLen)
        this.other = newOther
        newOther
      }
    }

    final def push(r: Runnable): Unit = {
      val sz = this.size
      if(sz == 0)
        this.first = r
      else
        ensureCapacity(sz)(sz - 1) = r
      this.size = sz + 1
    }

    @tailrec protected final def runN(n: Int): Unit =
      if (n > 0)
        (this.size: @switch) match {
          case 0 =>
          case 1 =>
            val next = this.first
            this.first = null
            this.size = 0
            next.run()
            runN(n - 1)
          case sz =>
            val o = this.other
            val next = o(sz - 2)
            o(sz - 2) = null
            this.size = sz - 1
            next.run()
            runN(n - 1)
          }
  }

  private[this] final class AsyncBatch private(_first: Runnable, _other: Array[Runnable], _size: Int) extends AbstractBatch(_first, _other, _size) with Runnable with BlockContext with (BlockContext => Throwable) {
    private[this] final var parentBlockContext: BlockContext = BatchingExecutorStatics.MissingParentBlockContext

    final def this(runnable: Runnable) = this(runnable, BatchingExecutorStatics.emptyBatchArray, 1)

    override final def run(): Unit = {
      _tasksLocal.set(this) // This is later cleared in `apply` or `runWithoutResubmit`

      val f = resubmit(BlockContext.usingBlockContext(this)(this))

      if (f != null)
        throw f
    }

    /* LOGIC FOR ASYNCHRONOUS BATCHES */
    override final def apply(prevBlockContext: BlockContext): Throwable = try {
      parentBlockContext = prevBlockContext
      runN(BatchingExecutorStatics.runLimit)
      null
    } catch {
      case t: Throwable => t // We are handling exceptions on the outside of this method
    } finally {
      parentBlockContext = BatchingExecutorStatics.MissingParentBlockContext
      _tasksLocal.remove()
    }

    /* Attempts to resubmit this Batch to the underlying ExecutionContext,
     * this only happens for Batches where `resubmitOnBlock` is `true`.
     * Only attempt to resubmit when there are `Runnables` left to process.
     * Note that `cause` can be `null`.
     */
    private[this] final def resubmit(cause: Throwable): Throwable =
      if (this.size > 0) {
        try { submitForExecution(this); cause } catch {
          case inner: Throwable =>
            if (NonFatal(inner)) {
              val e = new ExecutionException("Non-fatal error occurred and resubmission failed, see suppressed exception.", cause)
              e.addSuppressed(inner)
              e
            } else inner
        }
      } else cause // TODO: consider if NonFatals should simply be `reportFailure`:ed rather than rethrown

    private[this] final def cloneAndClear(): AsyncBatch = {
      val newBatch = new AsyncBatch(this.first, this.other, this.size)
      this.first = null
      this.other = BatchingExecutorStatics.emptyBatchArray
      this.size = 0
      newBatch
    }

    override final def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      // If we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
      if(this.size > 0)
        submitForExecution(cloneAndClear()) // If this throws then we have bigger problems

      parentBlockContext.blockOn(thunk) // Now delegate the blocking to the previous BC
    }
  }

  private[this] final class SyncBatch(runnable: Runnable) extends AbstractBatch(runnable, BatchingExecutorStatics.emptyBatchArray, 1) with Runnable {
    @tailrec override final def run(): Unit = {
      try runN(BatchingExecutorStatics.runLimit) catch {
        case ie: InterruptedException =>
          reportFailure(ie) // TODO: Handle InterruptedException differently?
        case f if NonFatal(f) =>
          reportFailure(f)
      }

      if (this.size > 0)
        run()
    }
  }

  /** MUST throw a NullPointerException when `runnable` is null
   * When implementing a sync BatchingExecutor, it is RECOMMENDED
   * to implement this method as `runnable.run()`
  */
  protected def submitForExecution(runnable: Runnable): Unit

  /** Reports that an asynchronous computation failed.
   *  See `ExecutionContext.reportFailure(throwable: Throwable)`
  */
  protected def reportFailure(throwable: Throwable): Unit

  /**
   * WARNING: Never use both `submitAsyncBatched` and `submitSyncBatched` in the same
   * implementation of `BatchingExecutor`
   */
  protected final def submitAsyncBatched(runnable: Runnable): Unit = {
    val b = _tasksLocal.get
    if (b.isInstanceOf[AsyncBatch]) b.asInstanceOf[AsyncBatch].push(runnable)
    else submitForExecution(new AsyncBatch(runnable))
  }

  /**
   * WARNING: Never use both `submitAsyncBatched` and `submitSyncBatched` in the same
   * implementation of `BatchingExecutor`
   */
  protected final def submitSyncBatched(runnable: Runnable): Unit = {
    Objects.requireNonNull(runnable, "runnable is null")
    val tl = _tasksLocal
    val b = tl.get
    if (b.isInstanceOf[SyncBatch]) b.asInstanceOf[SyncBatch].push(runnable)
    else {
      val i = if (b ne null) b.asInstanceOf[java.lang.Integer].intValue else 0
      if (i < BatchingExecutorStatics.syncPreBatchDepth) {
        tl.set(java.lang.Integer.valueOf(i + 1))
        try submitForExecution(runnable) // User code so needs to be try-finally guarded here
        catch {
          case ie: InterruptedException =>
            reportFailure(ie) // TODO: Handle InterruptedException differently?
          case f if NonFatal(f) =>
            reportFailure(f)
        }
        finally tl.set(b)
      } else {
        val batch = new SyncBatch(runnable)
        tl.set(batch)
        submitForExecution(batch)
        tl.set(b) // Batch only throws fatals so no need for try-finally here
      }
    }
  }
}
