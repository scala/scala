/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import scala.concurrent.util.Duration
import scala.concurrent.{Awaitable, ExecutionContext, CanAwait}
import scala.collection.mutable.Stack


private[concurrent] trait Future[+T] extends scala.concurrent.Future[T] with Awaitable[T] {

  implicit def executor: ExecutionContext

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

  /** Wraps a block of code into an awaitable object. */
  private[concurrent] def body2awaitable[T](body: =>T) = new Awaitable[T] {
    def ready(atMost: Duration)(implicit permit: CanAwait) = {
      body
      this
    }
    def result(atMost: Duration)(implicit permit: CanAwait) = body
  }
  
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
 
}
