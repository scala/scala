/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.Executor
import scala.annotation.tailrec

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

  // invariant: if "_tasksLocal.get ne null" then we are inside BatchingRunnable.run; if it is null, we are outside
  private val _tasksLocal = new ThreadLocal[List[Runnable]]()

  private class Batch(val initial: List[Runnable]) extends Runnable with BlockContext {
    private var parentBlockContext: BlockContext = _
    // this method runs in the delegate ExecutionContext's thread
    override def run(): Unit = {
      require(_tasksLocal.get eq null)

      val prevBlockContext = BlockContext.current
      BlockContext.withBlockContext(this) {
        try {
          parentBlockContext = prevBlockContext

          @tailrec def processBatch(batch: List[Runnable]): Unit = batch match {
            case Nil => ()
            case head :: tail =>
              _tasksLocal set tail
              try {
                head.run()
              } catch {
                case t: Throwable =>
                  // if one task throws, move the
                  // remaining tasks to another thread
                  // so we can throw the exception
                  // up to the invoking executor
                  val remaining = _tasksLocal.get
                  _tasksLocal set Nil
                  unbatchedExecute(new Batch(remaining)) //TODO what if this submission fails?
                  throw t // rethrow
              }
              processBatch(_tasksLocal.get) // since head.run() can add entries, always do _tasksLocal.get here
          }

          processBatch(initial)
        } finally {
          _tasksLocal.remove()
          parentBlockContext = null
        }
      }
    }

    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      // if we know there will be blocking, we don't want to keep tasks queued up because it could deadlock.
      {
        val tasks = _tasksLocal.get
        _tasksLocal set Nil
        if ((tasks ne null) && tasks.nonEmpty)
          unbatchedExecute(new Batch(tasks))
      }

      // now delegate the blocking to the previous BC
      require(parentBlockContext ne null)
      parentBlockContext.blockOn(thunk)
    }
  }

  protected def unbatchedExecute(r: Runnable): Unit

  override def execute(runnable: Runnable): Unit = {
    if (batchable(runnable)) { // If we can batch the runnable
      _tasksLocal.get match {
        case null => unbatchedExecute(new Batch(List(runnable))) // If we aren't in batching mode yet, enqueue batch
        case some => _tasksLocal.set(runnable :: some) // If we are already in batching mode, add to batch
      }
    } else unbatchedExecute(runnable) // If not batchable, just delegate to underlying
  }

  /** Override this to define which runnables will be batched. */
  def batchable(runnable: Runnable): Boolean = runnable match {
    case _: OnCompleteRunnable => true
    case _                     => false
  }
}
