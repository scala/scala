/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import scala.concurrent.{Awaitable, ExecutionContext}
import scala.collection.mutable.Stack

private[concurrent] trait Future[+T] extends scala.concurrent.Future[T] with Awaitable[T] {

  protected implicit def executor: ExecutionContext //This needs to be protected, and not public (Java)

  /** For use only within a Future.flow block or another compatible Delimited Continuations reset block.
   *
   *  Returns the result of this Future without blocking, by suspending execution and storing it as a
   *  continuation until the result is available.
   */
  //def apply(): T @cps[Future[Any]] = shift(this flatMap (_: T => Future[Any]))

  /** Tests whether this Future has been completed.
   */
  final def isCompleted: Boolean = value.isDefined

  /** The contained value of this Future. Before this Future is completed
   *  the value will be None. After completion the value will be Some(Right(t))
   *  if it contains a valid result, or Some(Left(error)) if it contains
   *  an exception.
   */
  def value: Option[Either[Throwable, T]]

  def onComplete[U](func: Either[Throwable, T] => U): this.type

}

object Future {
  import java.{ lang => jl }

  private val toBoxed = Map[Class[_], Class[_]](
    classOf[Boolean] -> classOf[jl.Boolean],
    classOf[Byte]    -> classOf[jl.Byte],
    classOf[Char]    -> classOf[jl.Character],
    classOf[Short]   -> classOf[jl.Short],
    classOf[Int]     -> classOf[jl.Integer],
    classOf[Long]    -> classOf[jl.Long],
    classOf[Float]   -> classOf[jl.Float],
    classOf[Double]  -> classOf[jl.Double],
    classOf[Unit]    -> classOf[scala.runtime.BoxedUnit]
  )

  def boxedType(c: Class[_]): Class[_] = { 
    if (c.isPrimitive) toBoxed(c) else c
  }
  
  def apply[T](body: =>T)(implicit executor: ExecutionContext): Future[T] = {
    val promise = new Promise.DefaultPromise[T]()
    executor.execute(new Runnable {
      def run = {
        promise complete {
          try {
            Right(body)
          } catch {
            case e => scala.concurrent.resolver(e)
          }
        }
      }
    })
    promise.future
  }

  // an optimization for batching futures
  // TODO we should replace this with a public queue,
  // so that it can be stolen from
  // OR: a push to the local task queue should be so cheap that this is
  // not even needed, but stealing is still possible
  private val _taskStack = new ThreadLocal[Stack[() => Unit]]()

  // 
  private[impl] def releaseStack(executor: ExecutionContext): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && stack.nonEmpty =>
        val tasks = stack.elems
        stack.clear()
        _taskStack.remove()
        dispatchFuture(executor, () => _taskStack.get.elems = tasks, true)
      case null =>
        // do nothing - there is no local batching stack anymore
      case _ =>
        _taskStack.remove()
    }

  private[impl] def dispatchFuture(executor: ExecutionContext, task: () => Unit, force: Boolean = false): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && !force => stack push task
      case _ => executor.execute(new Runnable {
        def run() {
          try {
            val taskStack = Stack[() => Unit](task)
            _taskStack set taskStack
            while (taskStack.nonEmpty) {
              val next = taskStack.pop()
              try {
                next.apply()
              } catch {
                case e =>
                  // TODO catching all and continue isn't good for OOME
                  executor.reportFailure(e)
              }
            }
          } finally {
            _taskStack.remove()
          }
        }
      })
    }


    // This is the current version of the methods above, that parries for mixing ExecutionContexts without b0rkenness,
    // So these ones below need to supercede the ones above.

    /**
   * Signals that the current thread of execution will potentially engage
   * an action that will take a non-trivial amount of time, perhaps by using blocking.IO or using a lot of CPU time,
   * giving the system a chance to spawn new threads, reuse old threads or otherwise,
   * to prevent starvation and/or unfairness.
   *
   * Assures that any Future tasks initiated in the current thread will be
   * executed asynchronously, including any tasks currently queued to be
   * executed in the current thread. This is needed if the current task may
   * block, causing delays in executing the remaining tasks which in some
   * cases may cause a deadlock.
   *
   * Note: Calling 'Await.result(future)' or 'Await.ready(future)' will automatically trigger this method.
   *
   * For example, in the following block of code the call to 'latch.open'
   * might not be executed until after the call to 'latch.await', causing
   * a deadlock. By adding 'Future.blocking()' the call to 'latch.open'
   * will instead be dispatched separately from the current block, allowing
   * it to be run in parallel:
   * <pre>
   * val latch = new StandardLatch
   * val future = Future() map { _ ⇒
   *   Future.blocking()
   *   val nested = Future()
   *   nested foreach (_ ⇒ latch.open)
   *   latch.await
   * }
   * </pre>
   */
  /*def blocking(): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && stack.nonEmpty ⇒
        val executionContext = _executionContext.value match {
          case null ⇒ throw new IllegalStateException("'blocking' needs to be invoked inside a Future callback.")
          case some ⇒ some
        }
        val tasks = stack.elems
        stack.clear()
        _taskStack.remove()
        dispatchTask(() ⇒ _taskStack.get.elems = tasks, true)(executionContext)
      case _ ⇒ _taskStack.remove()
    }

  private val _taskStack = new ThreadLocal[Stack[() ⇒ Unit]]()
  private val _executionContext = new DynamicVariable[ExecutionContext](null) // This should be replaced by the "currentExecutionContext" ThreadLocal
  */
  /**
   * Internal API, do not call
   */
  /*private[akka] def dispatchTask(task: () ⇒ Unit, force: Boolean = false)(implicit executor: ExecutionContext): Unit =
    _taskStack.get match {
      case stack if (stack ne null) && (executor eq _executionContext.value) && !force ⇒ stack push task
      case _ ⇒ executor.execute(
        new Runnable {
          def run =
            try {
              _executionContext.withValue(executor) {
                val taskStack = Stack.empty[() ⇒ Unit]
                taskStack push task
                _taskStack set taskStack

                while (taskStack.nonEmpty) {
                  val next = taskStack.pop()
                  try {
                    next.apply()
                  } catch {
                    case NonFatal(e) ⇒ executor.reportFailure(e)
                  }
                }
              }
            } finally {
              _taskStack.remove()
            }
        })
    }*/
 
}
