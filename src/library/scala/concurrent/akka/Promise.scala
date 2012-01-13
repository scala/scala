/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.akka



import scala.concurrent.{ExecutionContext, resolver}
import scala.util.continuations._



trait Promise[T] extends scala.concurrent.Promise[T] with Future[T] {
  
  // TODO refine answer and return types here from Any to type parameters
  
  final def <<(value: T): Future[T] @cps[Future[Any]] = shift {
    cont: (Future[T] => Future[Any]) =>
    cont(complete(Right(value)))
  }

  final def <<(other: Future[T]): Future[T] @cps[Future[Any]] = shift {
    cont: (Future[T] => Future[Any]) =>
    val p = executor.promise[Any]
    val thisPromise = this
    
    thisPromise completeWith other
    thisPromise onComplete { v =>
      try {
        p completeWith cont(thisPromise)
      } catch {
        case e => p complete resolver(e)
      }
    }
    
    p.future
  }
  
  // TODO finish this once we introduce something like dataflow streams
  
  /*
  final def <<(stream: PromiseStreamOut[T]): Future[T] @cps[Future[Any]] = shift { cont: (Future[T] => Future[Any]) =>
    val fr = executor.promise[Any]
    val f = stream.dequeue(this)
    f.onComplete { _ =>
      try {
        fr completeWith cont(f)
      } catch {
        case e =>
          fr failure e
      }
    }
    fr
  }
  */
  
}

