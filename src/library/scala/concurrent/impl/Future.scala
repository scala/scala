/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import scala.concurrent.{Awaitable, ExecutionContext}
import scala.util.{ Try, Success, Failure }
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
  def value: Option[Try[T]]

  def onComplete[U](func: Try[T] => U): this.type

  /** Creates a new Future[A] which is completed with this Future's result if
   *  that conforms to A's erased type or a ClassCastException otherwise.
   */
  final def mapTo[T](implicit m: Manifest[T]) = {
    val p = new Promise.DefaultPromise[T]

    onComplete {
      case f @ Failure(t) => p complete f.asInstanceOf[Try[T]]
      case Success(v) =>
        p complete (try {
          Success(Future.boxedType(m.erasure).cast(v).asInstanceOf[T])
        } catch {
          case e: ClassCastException => Failure(e)
        })
    }

    p.future
  }

  /** Used by for-comprehensions.
   */
  final def withFilter(p: T => Boolean) = new FutureWithFilter[T](this, p)

  final class FutureWithFilter[+A](self: Future[A], p: A => Boolean) {
    def foreach(f: A => Unit): Unit = self filter p foreach f
    def map[B](f: A => B) = self filter p map f
    def flatMap[B](f: A => Future[B]) = self filter p flatMap f
    def withFilter(q: A => Boolean): FutureWithFilter[A] = new FutureWithFilter[A](self, x ⇒ p(x) && q(x))
  }

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
            Success(body)
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
