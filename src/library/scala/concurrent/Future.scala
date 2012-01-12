/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import scala.util.{ Timeout, Duration }
import scala.Option

import java.util.concurrent.{ ConcurrentLinkedQueue, TimeUnit, Callable }
import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
import java.lang.{ Iterable => JIterable }
import java.util.{ LinkedList => JLinkedList }

import scala.annotation.tailrec
import scala.collection.mutable.Stack
import java.{ lang => jl }
import java.util.concurrent.atomic.{ AtomicReferenceFieldUpdater, AtomicInteger, AtomicBoolean }
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom



/** The trait that represents futures.
 *  
 *  @define multipleCallbacks
 *  Multiple callbacks may be registered; there is no guarantee that they will be
 *  executed in a particular order.
 *
 *  @define caughtThrowables
 *  The future may contain a throwable object and this means that the future failed.
 *  Futures obtained through combinators have the same exception as the future they were obtained from.
 *  The following throwable objects are not contained in the future:
 *  - Error - errors are not contained within futures
 *  - scala.util.control.ControlThrowable - not contained within futures
 *  - InterruptedException - not contained within futures
 *  
 *  Instead, the future is completed with a ExecutionException with one of the exceptions above
 *  as the cause.
 *
 *  @define forComprehensionExamples
 *  Example:
 *  
 *  {{{
 *  val f = future { 5 }
 *  val g = future { 3 }
 *  val h = for {
 *    x: Int <- f // returns Future(5)
 *    y: Int <- g // returns Future(5)
 *  } yield x + y
 *  }}}
 *  
 *  is translated to:
 *  
 *  {{{
 *  f flatMap { (x: Int) => g map { (y: Int) => x + y } }
 *  }}}
 *  
 *  @define nonDeterministic
 *  Note: using this method yields nondeterministic dataflow programs.
 */
trait Future[+T] extends Awaitable[T] {
self =>
  
  /* Callbacks */
  
  /** When this future is completed successfully (i.e. with a value),
   *  apply the provided partial function to the value if the partial function
   *  is defined at that value.
   *  
   *  If the future has already been completed with a value,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  $multipleCallbacks
   */
  def onSuccess[U](pf: PartialFunction[T, U]): this.type = onComplete {
    case Left(t) => // do nothing
    case Right(v) if pf isDefinedAt v => pf(v)
  }
  
  /** When this future is completed with a failure (i.e. with a throwable),
   *  apply the provided callback to the throwable.
   *  
   *  $caughtThrowables
   *  
   *  If the future has already been completed with a failure,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  Will not be called in case that the future is completed with a value.
   *  
   *  $multipleCallbacks
   */
  def onFailure[U](callback: PartialFunction[Throwable, U]): this.type = onComplete {
    case Left(t) if isFutureThrowable(t) => if (callback.isDefinedAt(t)) callback(t)
    case Right(v) => // do nothing
  }
  
  /** When this future is completed, either through an exception, a timeout, or a value,
   *  apply the provided function.
   *  
   *  If the future has already been completed,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  $multipleCallbacks
   */
  def onComplete[U](func: Either[Throwable, T] => U): this.type
  
  
  /* Miscellaneous */
  
  /** The execution context of the future.
   */
  def executor: ExecutionContext
  
  /** Creates a new promise.
   */
  def newPromise[S]: Promise[S] = executor promise
  
  
  /* Projections */
  
  /** Returns a failed projection of this future.
   *  
   *  The failed projection is a future holding a value of type `Throwable`.
   *  
   *  It is completed with a value which is the throwable of the original future
   *  in case the original future is failed.
   *  
   *  It is failed with a `NoSuchElementException` if the original future is completed successfully.
   *  
   *  Blocking on this future returns a value if the original future is completed with an exception
   *  and throws a corresponding exception if the original future fails.
   */
  def failed: Future[Throwable] = new Future[Throwable] {
    def executor = self.executor
    def onComplete[U](func: Either[Throwable, Throwable] => U) = {
      self.onComplete {
        case Left(t) => func(Right(t))
        case Right(v) => func(Left(noSuchElem(v))) // do nothing
      }
      this
    }
    def await(timeout: Timeout)(implicit canawait: CanAwait): Throwable = {
      var t: Throwable = null
      try {
        val res = self.await(timeout)
        t = noSuchElem(res)
      } catch {
        case t: Throwable => return t
      }
      throw t
    }
    private def noSuchElem(v: T) = 
      new NoSuchElementException("Future.failed not completed with a throwable. Instead completed with: " + v)
  }
  
  
  /* Monadic operations */
  
  /** Creates a new future that will handle any matching throwable that this
   *  future might contain. If there is no match, or if this future contains
   *  a valid result then the new future will contain the same.
   *  
   *  Example:
   *  
   *  {{{
   *  future (6 / 0) recover { case e: ArithmeticException ⇒ 0 } // result: 0
   *  future (6 / 0) recover { case e: NotFoundException   ⇒ 0 } // result: exception
   *  future (6 / 2) recover { case e: ArithmeticException ⇒ 0 } // result: 3
   *  }}}
   */
  def recover[U >: T](pf: PartialFunction[Throwable, U]): Future[U] = {
    val p = newPromise[U]
    
    onComplete {
      case Left(t) => if (pf isDefinedAt t) p success pf(t) else p failure t
      case Right(v) => p success v
    }
    
    p.future
  }
  
  /** Asynchronously processes the value in the future once the value becomes available.
   *  
   *  Will not be called if the future fails.
   */
  def foreach[U](f: T => U): Unit = onComplete {
    case Right(r) => f(r)
    case Left(_)  => // do nothing
  }
  
  /** Creates a new future by applying a function to the successful result of
   *  this future. If this future is completed with an exception then the new
   *  future will also contain this exception.
   *  
   *  $forComprehensionExample
   */
  def map[S](f: T => S): Future[S] = {
    val p = newPromise[S]
    
    onComplete {
      case Left(t) => p failure t
      case Right(v) => p success f(v)
    }
    
    p.future
  }
  
  /** Creates a new future by applying a function to the successful result of
   *  this future, and returns the result of the function as the new future.
   *  If this future is completed with an exception then the new future will
   *  also contain this exception.
   *  
   *  $forComprehensionExample
   */
  def flatMap[S](f: T => Future[S]): Future[S] = {
    val p = newPromise[S]
    
    onComplete {
      case Left(t) => p failure t
      case Right(v) => f(v) onComplete {
        case Left(t) => p failure t
        case Right(v) => p success v
      }
    }
    
    p.future
  }
  
  /** Creates a new future by filtering the value of the current future with a predicate.
   *  
   *  If the current future contains a value which satisfies the predicate, the new future will also hold that value.
   *  Otherwise, the resulting future will fail with a `NoSuchElementException`.
   *  
   *  If the current future fails or times out, the resulting future also fails or times out, respectively.
   *
   *  Example:
   *  {{{
   *  val f = future { 5 }
   *  val g = f filter { _ % 2 == 1 }
   *  val h = f filter { _ % 2 == 0 }
   *  await(0) g // evaluates to 5
   *  await(0) h // throw a NoSuchElementException
   *  }}}
   */
  def filter(pred: T => Boolean): Future[T] = {
    val p = newPromise[T]
    
    onComplete {
      case Left(t) => p failure t
      case Right(v) => if (pred(v)) p success v else p failure new NoSuchElementException("Future.filter predicate is not satisfied by: " + v)
    }
    
    p.future
  }
  
  /** Creates a new future by mapping the value of the current future if the given partial function is defined at that value.
   *  
   *  If the current future contains a value for which the partial function is defined, the new future will also hold that value.
   *  Otherwise, the resulting future will fail with a `NoSuchElementException`.
   *
   *  If the current future fails or times out, the resulting future also fails or times out, respectively.
   *
   *  Example:
   *  {{{
   *  val f = future { -5 }
   *  val g = f collect {
   *    case x if x < 0 => -x
   *  }
   *  val h = f collect {
   *    case x if x > 0 => x * 2
   *  }
   *  await(0) g // evaluates to 5
   *  await(0) h // throw a NoSuchElementException
   *  }}}
   */
  def collect[S](pf: PartialFunction[T, S]): Future[S] = {
    val p = newPromise[S]
    
    onComplete {
      case Left(t) => p failure t
      case Right(v) => if (pf.isDefinedAt(v)) p success pf(v) else p failure new NoSuchElementException("Future.collect partial function is not defined at: " + v)
    }
    
    p.future
  }
  
  /** Creates a new future which holds the result of this future if it was completed successfully, or, if not,
   *  the result of the `that` future if `that` is completed successfully.
   *  If both futures are failed, the resulting future holds the throwable object of the first future.
   *  
   *  Example:
   *  {{{
   *  val f = future { sys.error("failed") }
   *  val g = future { 5 }
   *  val h = f orElse g
   *  await(0) h // evaluates to 5
   *  }}}
   */
  def orElse[U >: T](that: Future[U]): Future[U] = {
    val p = newPromise[U]
    
    onComplete {
      case Left(t) => that onComplete {
        case Left(_) => p failure t
        case Right(v) => p success v
      }
      case Right(v) => p success v
    }
    
    p.future
  }
  
  /** Creates a new future which holds the result of either this future or `that` future, depending on
   *  which future was completed first.
   *  
   *  $nonDeterministic
   *  
   *  Example:
   *  {{{
   *  val f = future { sys.error("failed") }
   *  val g = future { 5 }
   *  val h = f orElse g
   *  await(0) h // evaluates to either 5 or throws a runtime exception
   *  }}}
   */
  def or[U >: T](that: Future[U]): Future[U] = {
    val p = newPromise[U]
    
    val completePromise: PartialFunction[Either[Throwable, T], _] = {
      case Left(t) => p tryFailure t
      case Right(v) => p trySuccess v
    }
    this onComplete completePromise
    this onComplete completePromise
    
    p.future
  }
  
}


object Future {
  
  /*
  // TODO make more modular by encoding this within the execution context
  def all[T, Coll[X] <: Traversable[X]](futures: Coll[Future[T]])(implicit cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Future[Coll[T]] = {
    val builder = cbf(futures)
    val p: Promise[Coll[T]] = executor.promise[Coll[T]]
    
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
  */
  
  @inline def apply[T](body: =>T)(implicit executor: ExecutionContext): Future[T] = executor.future(body)
  
}
