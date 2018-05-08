/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.ArrayDeque
import java.util.concurrent.Executor
import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
 * Marker trait to indicate that a Runnable is Batchable by BatchingExecutors
 */
trait Batchable {
  self: Runnable =>
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

  private[this] final class Batch(capacity: Int) extends ArrayDeque[Runnable](capacity) with Runnable with BlockContext with (BlockContext => Unit) {
    private[this] final var parentBlockContext: BlockContext = _

    def this(r: Runnable, capacity: Int) = {
      this(capacity)
      addLast(r)
    }

    final def executor: BatchingExecutor = BatchingExecutor.this

    // this method runs in the delegate ExecutionContext's thread
    override final def run(): Unit = BlockContext.usingBlockContext(this)(this)

    override final def apply(prevBlockContext: BlockContext): Unit = {
      //This invariant needs to hold: require(_tasksLocal.get eq null)
      parentBlockContext = prevBlockContext
      try {
        _tasksLocal.set(this)
        runAll()
        _tasksLocal.remove() // Will be cleared in the throwing-case by runAll()
      } finally {
        parentBlockContext = null
      }
    }

    @tailrec private[this] final def runAll(): Unit = {
      val next = pollLast()
      if (next ne null) {
        try next.run() catch {
          case t: Throwable =>
            parentBlockContext = null // Need to reset this before re-submitting it
            _tasksLocal.remove() // If unbatchedExecute runs synchronously
            handleRunFailure(t)
         }
        runAll()
      }
    }

    private[this] final def handleRunFailure(cause: Throwable): Nothing =
      if (NonFatal(cause) || cause.isInstanceOf[InterruptedException]) {
        try unbatchedExecute(this) catch {
          case inner: Throwable =>
            if (NonFatal(inner)) {
              val e = new ExecutionException("Non-fatal error occurred and resubmission failed, see suppressed exception.", cause)
              e.addSuppressed(inner)
              throw e
            } else throw inner
        }
        throw cause
      } else throw cause

    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      val pbc = parentBlockContext
      if(!isEmpty) { // if we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
        val b = new Batch(math.max(4, this.size))
        b.addAll(this)
        this.clear()
        unbatchedExecute(b)
      }

      if (pbc ne null) pbc.blockOn(thunk) // now delegate the blocking to the previous BC
      else {
        try thunk finally throw new IllegalStateException("BUG in BatchingExecutor.Batch: parentBlockContext is null")
      }
    }
  }

  protected def unbatchedExecute(r: Runnable): Unit

  private[this] final def batchedExecute(runnable: Runnable): Unit = {
    val b = _tasksLocal.get
    if (b ne null) b.addLast(runnable)
    else unbatchedExecute(new Batch(runnable, 4))
  }

  override def execute(runnable: Runnable): Unit =
    if(batchable(runnable)) batchedExecute(runnable)
    else unbatchedExecute(runnable)

  /** Override this to define which runnables will be batched.
    * By default it tests the Runnable for being an instance of [Batchable].
   **/
  protected def batchable(runnable: Runnable): Boolean = runnable.isInstanceOf[Batchable]
}