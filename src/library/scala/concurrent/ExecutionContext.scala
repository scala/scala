/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.atomic.{ AtomicInteger }
import java.util.concurrent.{ Executors, Future => JFuture, Callable }
import scala.util.Duration
import scala.util.{ Try, Success, Failure }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }
import scala.collection.generic.CanBuildFrom
import collection._



trait ExecutionContext {

  protected implicit object CanAwaitEvidence extends CanAwait

  def execute(runnable: Runnable): Unit

  def execute[U](body: () => U): Unit

  def promise[T]: Promise[T]

  def future[T](body: Callable[T]): Future[T] = future(body.call())

  def future[T](body: => T): Future[T]

  def blocking[T](atMost: Duration)(body: =>T): T

  def blocking[T](awaitable: Awaitable[T], atMost: Duration): T

  def reportFailure(t: Throwable): Unit

  /* implementations follow */

  private implicit val executionContext = this

  def keptPromise[T](result: T): Promise[T] = {
    val p = promise[T]
    p success result
  }

  def brokenPromise[T](t: Throwable): Promise[T] = {
    val p = promise[T]
    p failure t
  }

  /** TODO some docs
   *
   */
  def all[T, Coll[X] <: Traversable[X]](futures: Coll[Future[T]])(implicit cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Future[Coll[T]] = {
    import nondeterministic._
    val buffer = new mutable.ArrayBuffer[T]
    val counter = new AtomicInteger(1) // how else could we do this?
    val p: Promise[Coll[T]] = promise[Coll[T]] // we need an implicit execctx in the signature
    var idx = 0

    def tryFinish() = if (counter.decrementAndGet() == 0) {
      val builder = cbf(futures)
      builder ++= buffer
      p success builder.result
    }

    for (f <- futures) {
      val currentIndex = idx
      buffer += null.asInstanceOf[T]
      counter.incrementAndGet()
      f onComplete {
        case Failure(t) =>
          p tryFailure t
        case Success(v) =>
          buffer(currentIndex) = v
        tryFinish()
      }
      idx += 1
    }

    tryFinish()

    p.future
  }

  /** TODO some docs
   *
   */
  def any[T](futures: Traversable[Future[T]]): Future[T] = {
    val p = promise[T]
    val completeFirst: Try[T] => Unit = elem => p tryComplete elem

    futures foreach (_ onComplete completeFirst)

    p.future
  }

  /** TODO some docs
   *
   */
  def find[T](futures: Traversable[Future[T]])(predicate: T => Boolean): Future[Option[T]] = {
    if (futures.isEmpty) Promise.kept[Option[T]](None).future
    else {
      val result = promise[Option[T]]
      val count = new AtomicInteger(futures.size)
      val search: Try[T] => Unit = {
        v => v match {
          case Success(r) => if (predicate(r)) result trySuccess Some(r)
          case _        =>
        }
        if (count.decrementAndGet() == 0) result trySuccess None
      }

      futures.foreach(_ onComplete search)

      result.future
    }
  }

}


sealed trait CanAwait



