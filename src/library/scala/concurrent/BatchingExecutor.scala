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
  final val marker = ""
  final object MissingParentBlockContext extends BlockContext {
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
 */
private[concurrent] trait BatchingExecutor extends Executor {
  private[this] final val _tasksLocal = new ThreadLocal[AnyRef]()

  /*
   * Batch implements a LIFO queue (stack) and is used as a trampolining Runnable.
   * In order to conserve allocations, the first element in the batch is stored "unboxed" in
   * the `first` field. Subsequent Runnables are stored in the array called `other`.
  */
  private[this] final class Batch(private[this] final val resubmitOnBlock: Boolean) extends Runnable with BlockContext with (BlockContext => Throwable) {
    private[this] final var parentBlockContext: BlockContext = BatchingExecutorStatics.MissingParentBlockContext
    private[this] final var first: Runnable = _
    private[this] final var size: Int = _
    private[this] final var other: Array[Runnable] = BatchingExecutorStatics.emptyBatchArray

    def this(r: Runnable, resubmitOnBlock: Boolean) = {
      this(resubmitOnBlock)
      first = r
      size = 1
    }

    private def this(first: Runnable, other: Array[Runnable], size: Int, resubmitOnBlock: Boolean) = {
      this(resubmitOnBlock)
      this.first = first
      this.other = other
      this.size = size
    }

    private[this] final def cloneAndClear(): Batch = {
      val newBatch = new Batch(first, other, size, resubmitOnBlock)
      this.first = null
      this.parentBlockContext = BatchingExecutorStatics.MissingParentBlockContext
      this.other = BatchingExecutorStatics.emptyBatchArray
      this.size = 0
      newBatch
    }

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

    private[this] final def runNext(): Boolean =
      (this.size: @switch) match {
        case 0 => false
        case 1 =>
          val next = this.first
          this.first = null
          this.size = 0
          next.run()
          this.size > 0// Could have changed during next.run()
        case sz =>
          val o = this.other
          val next = o(sz - 2)
          o(sz - 2) = null
          this.size = sz - 1// Important to update prior to `r.run()`
          next.run()
          this.size > 0// Could have changed during next.run()
        }

    // This method runs in the delegate ExecutionContext's thread
    override final def run(): Unit = {
      _tasksLocal.set(this)

      val failure = // Only install the block context if we can resubmit on blocking
        if (resubmitOnBlock) BlockContext.usingBlockContext(this)(this)
        else runWithoutResubmit(runUntilFailureOrDone())

      _tasksLocal.set(BatchingExecutorStatics.marker)
      if (failure != null)
        throw handleRunFailure(failure)
    }

    override final def apply(prevBlockContext: BlockContext): Throwable = {
      parentBlockContext = prevBlockContext
      val failure = runUntilFailureOrDone()
      parentBlockContext = BatchingExecutorStatics.MissingParentBlockContext
      failure
    }

    @tailrec private[this] final def runWithoutResubmit(failure: Throwable): Throwable =
      if (failure != null && (failure.isInstanceOf[InterruptedException] || NonFatal(failure))) {
        reportFailure(failure)
        runWithoutResubmit(runUntilFailureOrDone())
      } else failure

    private[this] final def runUntilFailureOrDone(): Throwable =
      try {
        while(runNext()) {}

        null
      } catch {
        case t: Throwable => t
      }

    private[this] final def handleRunFailure(cause: Throwable): Throwable =
      if (resubmitOnBlock && size > 0) {
        try { submitAsync(this); cause } catch {
          case inner: Throwable =>
            if (NonFatal(inner)) {
              val e = new ExecutionException("Non-fatal error occurred and resubmission failed, see suppressed exception.", cause)
              e.addSuppressed(inner)
              e
            } else inner
        }
      } else cause

    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      val pbc = parentBlockContext // Store this for later since `cloneAndClear()` will reset it

      if(size > 0) // If we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
        submitAsync(cloneAndClear()) // If this throws then we have bigger problems

      pbc.blockOn(thunk) // Now delegate the blocking to the previous BC
    }
  }

  /** Schedules the `runnable` to be executed—will only be used if `isAsync` returns `true`.
  */
  protected def submitAsync(runnable: Runnable): Unit

  /** Returns whether this `Executor` runs on the calling thread or if it `submitAsync` will execute its `Runnable`:s asynchronously.
  */
  protected def isAsync: Boolean = true

  /** Must return `false` when `runnable` is `null`
  */
  protected def batchable(runnable: Runnable): Boolean = runnable.isInstanceOf[Batchable]

  /** Reports that an asynchronous computation failed.
  */
  protected def reportFailure(throwable: Throwable): Unit

  override final def execute(runnable: Runnable): Unit = {
    Objects.requireNonNull(runnable, "runnable is null")
    if (isAsync) {
      if (batchable(runnable)) {
        val b = _tasksLocal.get
        if (b.isInstanceOf[Batch]) b.asInstanceOf[Batch].push(runnable)
        else submitAsync(new Batch(runnable, resubmitOnBlock = true))
      } else submitAsync(runnable)
    } else {
      val b = _tasksLocal.get
      if (b.isInstanceOf[Batch]) b.asInstanceOf[Batch].push(runnable)
      else if (b == null) {                             // If there is null in _tasksLocal, set a marker and run, inflate the Batch only if needed
        _tasksLocal.set(BatchingExecutorStatics.marker) // Set a marker to indicate that we are submitting synchronously
        runnable.run()                                  // If we observe a non-null task which isn't a batch here, then allocate a batch
        _tasksLocal.remove()                            // Since we are executing synchronously, we can clear this at the end of execution
      } else new Batch(runnable, resubmitOnBlock = false).run()
    }
  }
}
