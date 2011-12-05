
/**
 *  Copyright (C) 2009-2011 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.concurrent

//import akka.AkkaException (replaced with Exception)
//import akka.event.Logging.Error (removed all logging)
import scala.util.{ Timeout, Duration }
import scala.Option
//import akka.japi.{ Procedure, Function ⇒ JFunc, Option ⇒ JOption } (commented methods)

import java.util.concurrent.{ ConcurrentLinkedQueue, TimeUnit, Callable }
import java.util.concurrent.TimeUnit.{ NANOSECONDS ⇒ NANOS, MILLISECONDS ⇒ MILLIS }
import java.lang.{ Iterable ⇒ JIterable }
import java.util.{ LinkedList ⇒ JLinkedList }

import scala.annotation.tailrec
import scala.collection.mutable.Stack
//import akka.util.Switch (commented method)
import java.{ lang ⇒ jl }
import java.util.concurrent.atomic.{ AtomicReferenceFieldUpdater, AtomicInteger, AtomicBoolean }



/** The trait that represents futures.
 *  
 *  @define futureTimeout
 *  The timeout of the future is:
 *  - if this future was obtained from a task (i.e. by calling `task.future`), the timeout associated with that task
 *  - if this future was obtained from a promise (i.e. by calling `promise.future`), the timeout associated with that promise
 *  - if this future was obtained from a combinator on some other future `g` (e.g. by calling `g.map(_)`), the timeout of `g`
 *  - if this future was obtained from a combinator on multiple futures `g0`, ..., `g1`, the minimum of the timeouts of these futures
 *
 *  @define multipleCallbacks
 *  Multiple callbacks may be registered; there is no guarantee that they will be
 *  executed in a particular order.
 *
 *  @define caughtThrowables
 *  The future may contain a throwable object and this means that the future failed.
 *  Futures obtained through combinators have the same exception as the future they were obtained from.
 *  The following throwable objects are treated differently:
 *  - Error - errors are not contained within futures
 *  - NonLocalControlException - not contained within futures
 *  - InterruptedException - not contained within futures
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
 */
trait Future[+T] extends Blockable[T] {
  
  /* Callbacks */
  
  /** When this future is completed successfully (i.e. with a value),
   *  apply the provided function to the value.
   *  
   *  If the future has already been completed with a value,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  Will not be called in case of a timeout.
   *  
   *  Will not be called in case of an exception.
   *  
   *  $multipleCallbacks
   */
  def onSuccess[U](func: T => U): this.type
  
  /** When this future is completed with a failure (i.e. with a throwable),
   *  apply the provided function to the throwable.
   *
   *  $caughtThrowables
   *  
   *  If the future has already been completed with a failure,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  Will not be called in case of a timeout.
   *  
   *  Will not be called in case of an exception.
   *  
   *  $multipleCallbacks
   */
  def onFailure[U](func: Throwable => U): this.type
  
  /** When this future times out, apply the provided function.
   *  
   *  If the future has already timed out,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  $multipleCallbacks
   */
  def onTimeout[U](func: => U): this.type
  
  /** When this future is completed, either through an exception, a timeout, or a value,
   *  apply the provided function.
   *  
   *  If the future has already been completed,
   *  this will either be applied immediately or be scheduled asynchronously.
   *  
   *  $multipleCallbacks
   */
  def onComplete[U](func: Either[Throwable, T] => U): this.type
  
  
  /* Various info */
  
  /** Tests whether this Future's timeout has expired.
   *
   *  $futureTimeout
   *  
   *  Note that an expired Future may still contain a value, or it may be
   *  completed with a value.
   */
  def isTimedout: Boolean
  
  /** This future's timeout.
   *  
   *  $futureTimeout
   */
  def timeout: Timeout
  
  /** This future's timeout in nanoseconds.
   *  
   *  $futureTimeout
   */
  def timeoutInNanos = if (timeout.duration.isFinite) timeout.duration.toNanos else Long.MaxValue
  
  
  /* Projections */
  
  def failed: Future[Exception]
  
  def timedout: Future[Timeout]
  
  
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
  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit timeout: Timeout): Future[U]
  
  /** Asynchronously processes the value in the future once the value becomes available.
   *  
   *  Will not be called if the future times out or fails.
   *  
   *  This method typically registers an `onResult` callback.
   */
  def foreach[U](f: T => U): Unit
  
  /** Creates a new future by applying a function to the successful result of
   *  this future. If this future is completed with an exception then the new
   *  future will also contain this exception.
   *  
   *  $forComprehensionExample
   */
  def map[S](f: T => S)(implicit timeout: Timeout): Future[S]
  
  /** Creates a new future by applying a function to the successful result of
   *  this future, and returns the result of the function as the new future.
   *  If this future is completed with an exception then the new future will
   *  also contain this exception.
   *  
   *  $forComprehensionExample
   */
  def flatMap[S](f: T => Future[S])(implicit timeout: Timeout): Future[S]
  
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
   *  val g = g filter { _ % 2 == 1 }
   *  val h = f filter { _ % 2 == 0 }
   *  block on g // evaluates to 5
   *  block on h // throw a NoSuchElementException
   *  }}}
   */
  def filter(p: T => Boolean)(implicit timeout: Timeout): Future[T]
  
}

class FutureTimeoutException(message: String, cause: Throwable = null) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
}
