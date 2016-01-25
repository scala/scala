/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.language.higherKinds

import java.util.concurrent.{ ConcurrentLinkedQueue, TimeUnit, Callable }
import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
import java.lang.{ Iterable => JIterable }
import java.util.{ LinkedList => JLinkedList }
import java.util.concurrent.atomic.{ AtomicReferenceFieldUpdater, AtomicInteger, AtomicLong, AtomicBoolean }

import scala.util.control.NonFatal
import scala.Option
import scala.util.{Try, Success, Failure}

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag



/** The trait that represents futures.
 *
 *  Asynchronous computations that yield futures are created with the `Future` call:
 *
 *  {{{
 *  val s = "Hello"
 *  val f: Future[String] = Future {
 *    s + " future!"
 *  }
 *  f onSuccess {
 *    case msg => println(msg)
 *  }
 *  }}}
 *
 *  @author  Philipp Haller, Heather Miller, Aleksandar Prokopec, Viktor Klang
 *
 *  @see [[http://docs.scala-lang.org/overviews/core/futures.html Futures and Promises]]
 *
 *  @define multipleCallbacks
 *  Multiple callbacks may be registered; there is no guarantee that they will be
 *  executed in a particular order.
 *
 *  @define caughtThrowables
 *  The future may contain a throwable object and this means that the future failed.
 *  Futures obtained through combinators have the same exception as the future they were obtained from.
 *  The following throwable objects are not contained in the future:
 *  - `Error` - errors are not contained within futures
 *  - `InterruptedException` - not contained within futures
 *  - all `scala.util.control.ControlThrowable` except `NonLocalReturnControl` - not contained within futures
 *
 *  Instead, the future is completed with a ExecutionException with one of the exceptions above
 *  as the cause.
 *  If a future is failed with a `scala.runtime.NonLocalReturnControl`,
 *  it is completed with a value from that throwable instead.
 *
 *  @define nonDeterministic
 *  Note: using this method yields nondeterministic dataflow programs.
 *
 *  @define forComprehensionExamples
 *  Example:
 *
 *  {{{
 *  val f = Future { 5 }
 *  val g = Future { 3 }
 *  val h = for {
 *    x: Int <- f // returns Future(5)
 *    y: Int <- g // returns Future(3)
 *  } yield x + y
 *  }}}
 *
 *  is translated to:
 *
 *  {{{
 *  f flatMap { (x: Int) => g map { (y: Int) => x + y } }
 *  }}}
 *
 * @define callbackInContext
 * The provided callback always runs in the provided implicit
 *`ExecutionContext`, though there is no guarantee that the
 * `execute()` method on the `ExecutionContext` will be called once
 * per callback or that `execute()` will be called in the current
 * thread. That is, the implementation may run multiple callbacks
 * in a batch within a single `execute()` and it may run
 * `execute()` either immediately or asynchronously.
 */
trait Future[+T] extends Awaitable[T] {

  // The executor within the lexical scope
  // of the Future trait. Note that this will
  // (modulo bugs) _never_ execute a callback
  // other than those below in this same file.
  //
  // See the documentation on `InternalCallbackExecutor` for more details.
  private def internalExecutor = Future.InternalCallbackExecutor

  /* Callbacks */

  /** When this future is completed successfully (i.e., with a value),
   *  apply the provided partial function to the value if the partial function
   *  is defined at that value.
   *
   *  If the future has already been completed with a value,
   *  this will either be applied immediately or be scheduled asynchronously.
   *
   *  $multipleCallbacks
   *  $callbackInContext
   */
  def onSuccess[U](pf: PartialFunction[T, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Success(v) =>
      pf.applyOrElse[T, Any](v, Predef.conforms[T]) // Exploiting the cached function to avoid MatchError
    case _ =>
  }

  /** When this future is completed with a failure (i.e., with a throwable),
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
   *  $callbackInContext
   */
  def onFailure[U](@deprecatedName('callback) pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Failure(t) =>
      pf.applyOrElse[Throwable, Any](t, Predef.conforms[Throwable]) // Exploiting the cached function to avoid MatchError
    case _ =>
  }

  /** When this future is completed, either through an exception, or a value,
   *  apply the provided function.
   *
   *  If the future has already been completed,
   *  this will either be applied immediately or be scheduled asynchronously.
   *
   *  $multipleCallbacks
   *  $callbackInContext
   */
  def onComplete[U](@deprecatedName('func) f: Try[T] => U)(implicit executor: ExecutionContext): Unit


  /* Miscellaneous */

  /** Returns whether the future has already been completed with
   *  a value or an exception.
   *
   *  $nonDeterministic
   *
   *  @return    `true` if the future is already completed, `false` otherwise
   */
  def isCompleted: Boolean

  /** The value of this `Future`.
   *
   *  If the future is not completed the returned value will be `None`.
   *  If the future is completed the value will be `Some(Success(t))`
   *  if it contains a valid result, or `Some(Failure(error))` if it contains
   *  an exception.
   */
  def value: Option[Try[T]]


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
  def failed: Future[Throwable] = {
    implicit val ec = internalExecutor
    val p = Promise[Throwable]()
    onComplete {
      case Failure(t) => p success t
      case Success(v) => p failure (new NoSuchElementException("Future.failed not completed with a throwable."))
    }
    p.future
  }


  /* Monadic operations */

  /** Asynchronously processes the value in the future once the value becomes available.
   *
   *  Will not be called if the future fails.
   */
  def foreach[U](f: T => U)(implicit executor: ExecutionContext): Unit = onComplete { _ foreach f }

  /** Creates a new future by applying the 's' function to the successful result of
   *  this future, or the 'f' function to the failed result. If there is any non-fatal
   *  exception thrown when 's' or 'f' is applied, that exception will be propagated
   *  to the resulting future.
   *
   *  @param  s  function that transforms a successful result of the receiver into a
   *             successful result of the returned future
   *  @param  f  function that transforms a failure of the receiver into a failure of
   *             the returned future
   *  @return    a future that will be completed with the transformed value
   */
  def transform[S](s: T => S, f: Throwable => Throwable)(implicit executor: ExecutionContext): Future[S] = {
    val p = Promise[S]()
    // transform on Try has the wrong shape for us here
    onComplete {
      case Success(r) => p complete Try(s(r))
      case Failure(t) => p complete Try(throw f(t)) // will throw fatal errors!
    }
    p.future
  }

  /** Creates a new future by applying a function to the successful result of
   *  this future. If this future is completed with an exception then the new
   *  future will also contain this exception.
   *
   *  $forComprehensionExamples
   */
  def map[S](f: T => S)(implicit executor: ExecutionContext): Future[S] = { // transform(f, identity)
    val p = Promise[S]()
    onComplete { v => p complete (v map f) }
    p.future
  }

  /** Creates a new future by applying a function to the successful result of
   *  this future, and returns the result of the function as the new future.
   *  If this future is completed with an exception then the new future will
   *  also contain this exception.
   *
   *  $forComprehensionExamples
   */
  def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = {
    import impl.Promise.DefaultPromise
    val p = new DefaultPromise[S]()
    onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[S]]
      case Success(v) => try f(v) match {
        // If possible, link DefaultPromises to avoid space leaks
        case dp: DefaultPromise[_] => dp.asInstanceOf[DefaultPromise[S]].linkRootOf(p)
        case fut => fut.onComplete(p.complete)(internalExecutor)
      } catch { case NonFatal(t) => p failure t }
    }
    p.future
  }

  /** Creates a new future by filtering the value of the current future with a predicate.
   *
   *  If the current future contains a value which satisfies the predicate, the new future will also hold that value.
   *  Otherwise, the resulting future will fail with a `NoSuchElementException`.
   *
   *  If the current future fails, then the resulting future also fails.
   *
   *  Example:
   *  {{{
   *  val f = Future { 5 }
   *  val g = f filter { _ % 2 == 1 }
   *  val h = f filter { _ % 2 == 0 }
   *  Await.result(g, Duration.Zero) // evaluates to 5
   *  Await.result(h, Duration.Zero) // throw a NoSuchElementException
   *  }}}
   */
  def filter(@deprecatedName('pred) p: T => Boolean)(implicit executor: ExecutionContext): Future[T] =
    map {
      r => if (p(r)) r else throw new NoSuchElementException("Future.filter predicate is not satisfied")
    }

  /** Used by for-comprehensions.
   */
  final def withFilter(p: T => Boolean)(implicit executor: ExecutionContext): Future[T] = filter(p)(executor)

  /** Creates a new future by mapping the value of the current future, if the given partial function is defined at that value.
   *
   *  If the current future contains a value for which the partial function is defined, the new future will also hold that value.
   *  Otherwise, the resulting future will fail with a `NoSuchElementException`.
   *
   *  If the current future fails, then the resulting future also fails.
   *
   *  Example:
   *  {{{
   *  val f = Future { -5 }
   *  val g = f collect {
   *    case x if x < 0 => -x
   *  }
   *  val h = f collect {
   *    case x if x > 0 => x * 2
   *  }
   *  Await.result(g, Duration.Zero) // evaluates to 5
   *  Await.result(h, Duration.Zero) // throw a NoSuchElementException
   *  }}}
   */
  def collect[S](pf: PartialFunction[T, S])(implicit executor: ExecutionContext): Future[S] =
    map {
      r => pf.applyOrElse(r, (t: T) => throw new NoSuchElementException("Future.collect partial function is not defined at: " + t))
    }

  /** Creates a new future that will handle any matching throwable that this
   *  future might contain. If there is no match, or if this future contains
   *  a valid result then the new future will contain the same.
   *
   *  Example:
   *
   *  {{{
   *  Future (6 / 0) recover { case e: ArithmeticException => 0 } // result: 0
   *  Future (6 / 0) recover { case e: NotFoundException   => 0 } // result: exception
   *  Future (6 / 2) recover { case e: ArithmeticException => 0 } // result: 3
   *  }}}
   */
  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] = {
    val p = Promise[U]()
    onComplete { v => p complete (v recover pf) }
    p.future
  }

  /** Creates a new future that will handle any matching throwable that this
   *  future might contain by assigning it a value of another future.
   *
   *  If there is no match, or if this future contains
   *  a valid result then the new future will contain the same result.
   *
   *  Example:
   *
   *  {{{
   *  val f = Future { Int.MaxValue }
   *  Future (6 / 0) recoverWith { case e: ArithmeticException => f } // result: Int.MaxValue
   *  }}}
   */
  def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] = {
    val p = Promise[U]()
    onComplete {
      case Failure(t) => try pf.applyOrElse(t, (_: Throwable) => this).onComplete(p.complete)(internalExecutor) catch { case NonFatal(t) => p failure t }
      case other => p complete other
    }
    p.future
  }

  /** Zips the values of `this` and `that` future, and creates
   *  a new future holding the tuple of their results.
   *
   *  If `this` future fails, the resulting future is failed
   *  with the throwable stored in `this`.
   *  Otherwise, if `that` future fails, the resulting future is failed
   *  with the throwable stored in `that`.
   */
  def zip[U](that: Future[U]): Future[(T, U)] = {
    implicit val ec = internalExecutor
    val p = Promise[(T, U)]()
    onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[(T, U)]]
      case Success(s) => that onComplete { c => p.complete(c map { s2 => (s, s2) }) }
    }
    p.future
  }

  /** Creates a new future which holds the result of this future if it was completed successfully, or, if not,
   *  the result of the `that` future if `that` is completed successfully.
   *  If both futures are failed, the resulting future holds the throwable object of the first future.
   *
   *  Using this method will not cause concurrent programs to become nondeterministic.
   *
   *  Example:
   *  {{{
   *  val f = Future { sys.error("failed") }
   *  val g = Future { 5 }
   *  val h = f fallbackTo g
   *  Await.result(h, Duration.Zero) // evaluates to 5
   *  }}}
   */
  def fallbackTo[U >: T](that: Future[U]): Future[U] = {
    implicit val ec = internalExecutor
    val p = Promise[U]()
    onComplete {
      case s @ Success(_) => p complete s
      case f @ Failure(_) => that onComplete {
        case s2 @ Success(_) => p complete s2
        case _ => p complete f // Use the first failure as the failure
      }
    }
    p.future
  }

  /** Creates a new `Future[S]` which is completed with this `Future`'s result if
   *  that conforms to `S`'s erased type or a `ClassCastException` otherwise.
   */
  def mapTo[S](implicit tag: ClassTag[S]): Future[S] = {
    implicit val ec = internalExecutor
    val boxedClass = {
      val c = tag.runtimeClass
      if (c.isPrimitive) Future.toBoxed(c) else c
    }
    require(boxedClass ne null)
    map(s => boxedClass.cast(s).asInstanceOf[S])
  }

  /** Applies the side-effecting function to the result of this future, and returns
   *  a new future with the result of this future.
   *
   *  This method allows one to enforce that the callbacks are executed in a
   *  specified order.
   *
   *  Note that if one of the chained `andThen` callbacks throws
   *  an exception, that exception is not propagated to the subsequent `andThen`
   *  callbacks. Instead, the subsequent `andThen` callbacks are given the original
   *  value of this future.
   *
   *  The following example prints out `5`:
   *
   *  {{{
   *  val f = Future { 5 }
   *  f andThen {
   *    case r => sys.error("runtime exception")
   *  } andThen {
   *    case Failure(t) => println(t)
   *    case Success(v) => println(v)
   *  }
   *  }}}
   */
  def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    onComplete {
      case r => try pf.applyOrElse[Try[T], Any](r, Predef.conforms[Try[T]]) finally p complete r
    }
    p.future
  }

}



/** Future companion object.
 *
 *  @define nonDeterministic
 *  Note: using this method yields nondeterministic dataflow programs.
 */
object Future {

  private[concurrent] val toBoxed = Map[Class[_], Class[_]](
    classOf[Boolean] -> classOf[java.lang.Boolean],
    classOf[Byte]    -> classOf[java.lang.Byte],
    classOf[Char]    -> classOf[java.lang.Character],
    classOf[Short]   -> classOf[java.lang.Short],
    classOf[Int]     -> classOf[java.lang.Integer],
    classOf[Long]    -> classOf[java.lang.Long],
    classOf[Float]   -> classOf[java.lang.Float],
    classOf[Double]  -> classOf[java.lang.Double],
    classOf[Unit]    -> classOf[scala.runtime.BoxedUnit]
  )

  /** Creates an already completed Future with the specified exception.
   *
   *  @tparam T       the type of the value in the future
   *  @return         the newly created `Future` object
   */
  def failed[T](exception: Throwable): Future[T] = Promise.failed(exception).future

  /** Creates an already completed Future with the specified result.
   *
   *  @tparam T       the type of the value in the future
   *  @return         the newly created `Future` object
   */
  def successful[T](result: T): Future[T] = Promise.successful(result).future

  /** Creates an already completed Future with the specified result or exception.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Future` object
   */
  def fromTry[T](result: Try[T]): Future[T] = Promise.fromTry(result).future

  /** Starts an asynchronous computation and returns a `Future` object with the result of that computation.
  *
  *  The result becomes available once the asynchronous computation is completed.
  *
  *  @tparam T       the type of the result
  *  @param body     the asynchronous computation
  *  @param executor  the execution context on which the future is run
  *  @return         the `Future` holding the result of the computation
  */
  def apply[T](body: =>T)(implicit @deprecatedName('execctx) executor: ExecutionContext): Future[T] = impl.Future(body)

  /** Simple version of `Future.traverse`. Transforms a `TraversableOnce[Future[A]]` into a `Future[TraversableOnce[A]]`.
   *  Useful for reducing many `Future`s into a single `Future`.
   */
  def sequence[A, M[X] <: TraversableOnce[X]](in: M[Future[A]])(implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]], executor: ExecutionContext): Future[M[A]] = {
    in.foldLeft(successful(cbf(in))) {
      (fr, fa) => for (r <- fr; a <- fa) yield (r += a)
    }.map(_.result())(InternalCallbackExecutor)
  }

  /** Returns a new `Future` to the result of the first future in the list that is completed.
   */
  def firstCompletedOf[T](futures: TraversableOnce[Future[T]])(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    val completeFirst: Try[T] => Unit = p tryComplete _
    futures foreach { _ onComplete completeFirst }
    p.future
  }

  /** Returns a `Future` that will hold the optional result of the first `Future` with a result that matches the predicate.
   */
  def find[T](@deprecatedName('futurestravonce) futures: TraversableOnce[Future[T]])(@deprecatedName('predicate) p: T => Boolean)(implicit executor: ExecutionContext): Future[Option[T]] = {
    val futuresBuffer = futures.toBuffer
    if (futuresBuffer.isEmpty) successful[Option[T]](None)
    else {
      val result = Promise[Option[T]]()
      val ref = new AtomicInteger(futuresBuffer.size)
      val search: Try[T] => Unit = v => try {
        v match {
          case Success(r) if p(r) => result tryComplete Success(Some(r))
          case _ =>
        }
      } finally {
        if (ref.decrementAndGet == 0) {
          result tryComplete Success(None)
        }
      }

      futuresBuffer.foreach(_ onComplete search)

      result.future
    }
  }

  /** A non-blocking fold over the specified futures, with the start value of the given zero.
   *  The fold is performed on the thread where the last future is completed,
   *  the result will be the first failure of any of the futures, or any failure in the actual fold,
   *  or the result of the fold.
   *
   *  Example:
   *  {{{
   *    val result = Await.result(Future.fold(futures)(0)(_ + _), 5 seconds)
   *  }}}
   */
  def fold[T, R](futures: TraversableOnce[Future[T]])(zero: R)(@deprecatedName('foldFun) op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) successful(zero)
    else sequence(futures).map(_.foldLeft(zero)(op))
  }

  /** Initiates a fold over the supplied futures where the fold-zero is the result value of the `Future` that's completed first.
   *
   *  Example:
   *  {{{
   *    val result = Await.result(Future.reduce(futures)(_ + _), 5 seconds)
   *  }}}
   */
  def reduce[T, R >: T](futures: TraversableOnce[Future[T]])(op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) failed(new NoSuchElementException("reduce attempted on empty collection"))
    else sequence(futures).map(_ reduceLeft op)
  }

  /** Transforms a `TraversableOnce[A]` into a `Future[TraversableOnce[B]]` using the provided function `A => Future[B]`.
   *  This is useful for performing a parallel map. For example, to apply a function to all items of a list
   *  in parallel:
   *
   *  {{{
   *    val myFutureList = Future.traverse(myList)(x => Future(myFunc(x)))
   *  }}}
   */
  def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Future[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]], executor: ExecutionContext): Future[M[B]] =
    in.foldLeft(successful(cbf(in))) { (fr, a) =>
      val fb = fn(a)
      for (r <- fr; b <- fb) yield (r += b)
    }.map(_.result())

  // This is used to run callbacks which are internal
  // to scala.concurrent; our own callbacks are only
  // ever used to eventually run another callback,
  // and that other callback will have its own
  // executor because all callbacks come with
  // an executor. Our own callbacks never block
  // and have no "expected" exceptions.
  // As a result, this executor can do nothing;
  // some other executor will always come after
  // it (and sometimes one will be before it),
  // and those will be performing the "real"
  // dispatch to code outside scala.concurrent.
  // Because this exists, ExecutionContext.defaultExecutionContext
  // isn't instantiated by Future internals, so
  // if some code for some reason wants to avoid
  // ever starting up the default context, it can do so
  // by just not ever using it itself. scala.concurrent
  // doesn't need to create defaultExecutionContext as
  // a side effect.
  private[concurrent] object InternalCallbackExecutor extends ExecutionContext with BatchingExecutor {
    override protected def unbatchedExecute(r: Runnable): Unit =
      r.run()
    override def reportFailure(t: Throwable): Unit =
      throw new IllegalStateException("problem in scala.concurrent internal callback", t)
  }
}

/** A marker indicating that a `java.lang.Runnable` provided to `scala.concurrent.ExecutionContext`
 * wraps a callback provided to `Future.onComplete`.
 * All callbacks provided to a `Future` end up going through `onComplete`, so this allows an
 * `ExecutionContext` to special-case callbacks that were executed by `Future` if desired.
 */
trait OnCompleteRunnable {
  self: Runnable =>
}

