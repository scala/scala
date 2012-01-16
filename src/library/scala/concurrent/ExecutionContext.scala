/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.{ Executors, Future => JFuture, Callable }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }
import scala.collection.generic.CanBuildFrom



trait ExecutionContext {
  
  protected implicit object CanAwaitEvidence extends CanAwait
  
  def execute(runnable: Runnable): Unit
  
  def execute[U](body: () => U): Unit
  
  def promise[T]: Promise[T]
  
  def future[T](body: Callable[T]): Future[T] = future(body.call())
  
  def future[T](body: => T): Future[T]
  
  def blocking[T](atMost: Duration)(body: =>T): T
  
  def blocking[T](awaitable: Awaitable[T], atMost: Duration): T
  
  def futureUtilities: FutureUtilities = FutureUtilitiesImpl
  
}


sealed trait CanAwait


trait FutureUtilities {
  
  def all[T, Coll[X] <: Traversable[X]](futures: Coll[Future[T]])(implicit cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Future[Coll[T]] = {
    val builder = cbf(futures)
    val p: Promise[Coll[T]] = promise[Coll[T]]
    
    if (futures.size == 1) futures.head onComplete {
      case Left(t) => p failure t
      case Right(v) => builder += v
        p success builder.result
    } else {
      val restFutures = all(futures.tail)
      futures.head onComplete {
        case Left(t) => p failure t
        case Right(v) => builder += v
          restFutures onComplete {
            case Left(t) => p failure t
            case Right(vs) => for (v <- vs) builder += v
              p success builder.result
          }
      }
    }
    
    p.future
  }
  
}


object FutureUtilitiesImpl extends FutureUtilities {
}

