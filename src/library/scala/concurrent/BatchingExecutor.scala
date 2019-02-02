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
import scala.annotation.{ switch, tailrec }
import scala.util.control.NonFatal

/**
 * Marker trait to indicate that a Runnable is Batchable by BatchingExecutors
 */
trait Batchable {
  self: Runnable =>
}

private[concurrent] object BatchingExecutorStatics {
  final val emptyBatchArray: Array[Runnable] = new Array[Runnable](0)
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
  private[this] final val _tasksLocal = new ThreadLocal[Batch]()

  private[this] final class Batch extends Runnable with BlockContext with (BlockContext => Throwable) {
    private[this] final var parentBlockContext: BlockContext = _
    private[this] final var first: Runnable = _
    private[this] final var size: Int = _
    private[this] final var other: Array[Runnable] = BatchingExecutorStatics.emptyBatchArray

    def this(r: Runnable) = {
      this()
      first = r
      size = 1
    }

    private def this(first: Runnable, other: Array[Runnable], size: Int) = {
      this()
      this.first = first
      this.other = other
      this.size = size
    }

    private[this] final def cloneAndClear(): Batch = {
      val newBatch = new Batch(first, other, size)
      this.first = null
      this.other = BatchingExecutorStatics.emptyBatchArray
      this.size = 0
      newBatch
    }

    private[this] final def grow(): Unit = {
      val len = other.length
      other =
        if (len == 0) new Array[Runnable](4)
        else {
          val newOther = new Array[Runnable](len << 1)
          System.arraycopy(other, 0, newOther, 0, len)
          newOther
        }
    }

    final def push(r: Runnable): Unit = {
      val sz = size
      if(sz > 0) {
        if (sz > other.length)
          grow()
        other(sz - 1) = r
      } else first = r
      size = sz + 1
    }

    final def pop(): Runnable =
      (size: @switch) match {
        case 0 => null
        case 1 =>
          val ret = first
          first = null
          size = 0
          ret
        case n =>
          val ret = other(n - 2)
          other(n - 2) = null
          size = n - 1
          ret
      }

    // this method runs in the delegate ExecutionContext's thread
    override final def run(): Unit = {
      //This invariant needs to hold: require(_tasksLocal.get eq null)
      _tasksLocal.set(this)
      val failure = BlockContext.usingBlockContext(this)(this)
      _tasksLocal.remove()
      if (failure ne null)
        throw handleRunFailure(failure)
    }

    override final def apply(prevBlockContext: BlockContext): Throwable = {
      parentBlockContext = prevBlockContext
      var failure: Throwable = null
      try {
        var r = pop()
        while(r ne null) {
          r.run()
          r = pop()
        }
      } catch {
        case t: Throwable => failure = t
      }
      parentBlockContext = null
      failure
    }

    private[this] final def handleRunFailure(cause: Throwable): Throwable =
      if (size > 0 && (NonFatal(cause) || cause.isInstanceOf[InterruptedException])) {
        try { unbatchedExecute(this); cause } catch {
          case inner: Throwable =>
            if (NonFatal(inner)) {
              val e = new ExecutionException("Non-fatal error occurred and resubmission failed, see suppressed exception.", cause)
              e.addSuppressed(inner)
              e
            } else inner
        }
      } else cause

    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      val pbc = parentBlockContext
      if(size > 0) // if we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
        unbatchedExecute(cloneAndClear())

      if (pbc ne null) pbc.blockOn(thunk) // now delegate the blocking to the previous BC
      else {
        try thunk finally throw new IllegalStateException("BUG in BatchingExecutor.Batch: parentBlockContext is null")
      }
    }
  }

  protected def unbatchedExecute(r: Runnable): Unit

  private[this] final def batchedExecute(runnable: Runnable): Unit = {
    val b = _tasksLocal.get
    if (b ne null) b.push(runnable)
    else unbatchedExecute(new Batch(runnable))
  }

  override def execute(runnable: Runnable): Unit =
    if(batchable(runnable)) batchedExecute(runnable)
    else unbatchedExecute(runnable)

  /** Override this to define which runnables will be batched.
    * By default it tests the Runnable for being an instance of [Batchable].
   **/
  protected def batchable(runnable: Runnable): Boolean = runnable.isInstanceOf[Batchable]
}
