/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.{ ConcurrentLinkedQueue, TimeUnit, Callable }
import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
import java.lang.{ Iterable => JIterable }
import java.util.{ LinkedList => JLinkedList }
import java.{ lang => jl }
import java.util.concurrent.atomic.{ AtomicReferenceFieldUpdater, AtomicInteger, AtomicBoolean }

import scala.concurrent.util.Duration
import scala.concurrent.impl.NonFatal
import scala.Option

import scala.annotation.tailrec
import scala.collection.mutable.Stack
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import language.higherKinds



/** The trait that represents futures.
 *
 *  Asynchronous computations that yield futures are created with the `future` call:
 *
 *  {{{
 *  val s = "Hello"
 *  val f: Future[String] = future {
 *    s + " future!"
 *  }
 *  f onSuccess {
 *    case msg => println(msg)
 *  }
 *  }}}
 *
 *  @author  Philipp Haller, Heather Miller, Aleksandar Prokopec, Viktor Klang
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
 *  it is completed with a value instead from that throwable instead instead.
 *
 *  @define nonDeterministic
 *  Note: using this method yields nondeterministic dataflow programs.
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
  // As a nice side benefit, having this implicit
  // here forces an ambiguity in those methods
  // that also have an executor parameter, which
  // keeps us from accidentally forgetting to use
  // the executor parameter.
  private implicit def internalExecutor: ExecutionContext = Future.InternalCallbackExecutor

  /* Callbacks */

  /** When this future is completed successfully (i.e. with a value),
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
    case Right(v) if pf isDefinedAt v => pf(v)
    case _ =>
  }(executor)

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
   *  $callbackInContext
   */
  def onFailure[U](callback: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Left(t) if (isFutureThrowable(t) && callback.isDefinedAt(t)) => callback(t)
    case _ =>
  }(executor)

  /** When this future is completed, either through an exception, or a value,
   *  apply the provided function.
   *
   *  If the future has already been completed,
   *  this will either be applied immediately or be scheduled asynchronously.
   *
   *  $multipleCallbacks
   *  $callbackInContext
   */
  def onComplete[U](func: Either[Throwable, T] => U)(implicit executor: ExecutionContext): Unit


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
  def value: Option[Either[Throwable, T]]


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
    val p = Promise[Throwable]()

    onComplete {
      case Left(t) => p success t
      case Right(v) => p failure (new NoSuchElementException("Future.failed not completed with a throwable."))
    }

    p.future
  }


  /* Monadic operations */

  /** Asynchronously processes the value in the future once the value becomes available.
   *
   *  Will not be called if the future fails.
   */
  def foreach[U](f: T => U)(implicit executor: ExecutionContext): Unit = onComplete {
    case Right(r) => f(r)
    case _  => // do nothing
  }(executor)

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

    onComplete {
      case result =>
        try {
          result match {
            case Left(t)  => p failure f(t)
            case Right(r) => p success s(r)
          }
        } catch {
          case NonFatal(t) => p failure t
        }
    }(executor)

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

    onComplete {
      case result =>
        try {
          result match {
            case Right(r) => p success f(r)
            case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, S]]
          }
        } catch {
          case NonFatal(t) => p failure t
        }
    }(executor)

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
    val p = Promise[S]()

    onComplete {
      case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, S]]
      case Right(v) =>
        try {
          f(v).onComplete({
            case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, S]]
            case Right(v) => p success v
          })(internalExecutor)
        } catch {
          case NonFatal(t) => p failure t
        }
    }(executor)

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
   *  val f = future { 5 }
   *  val g = f filter { _ % 2 == 1 }
   *  val h = f filter { _ % 2 == 0 }
   *  await(g, 0) // evaluates to 5
   *  await(h, 0) // throw a NoSuchElementException
   *  }}}
   */
  def filter(pred: T => Boolean)(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()

    onComplete {
      case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, T]]
      case Right(v) =>
        try {
          if (pred(v)) p success v
          else p failure new NoSuchElementException("Future.filter predicate is not satisfied by: " + v)
        } catch {
          case NonFatal(t) => p failure t
        }
    }(executor)

    p.future
  }

  /** Used by for-comprehensions.
   */
  final def withFilter(p: T => Boolean)(implicit executor: ExecutionContext): Future[T] = filter(p)(executor)
  // final def withFilter(p: T => Boolean) = new FutureWithFilter[T](this, p)

  // final class FutureWithFilter[+S](self: Future[S], p: S => Boolean) {
  //   def foreach(f: S => Unit): Unit = self filter p foreach f
  //   def map[R](f: S => R) = self filter p map f
  //   def flatMap[R](f: S => Future[R]) = self filter p flatMap f
  //   def withFilter(q: S => Boolean): FutureWithFilter[S] = new FutureWithFilter[S](self, x => p(x) && q(x))
  // }

  /** Creates a new future by mapping the value of the current future, if the given partial function is defined at that value.
   *
   *  If the current future contains a value for which the partial function is defined, the new future will also hold that value.
   *  Otherwise, the resulting future will fail with a `NoSuchElementException`.
   *
   *  If the current future fails, then the resulting future also fails.
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
   *  await(g, 0) // evaluates to 5
   *  await(h, 0) // throw a NoSuchElementException
   *  }}}
   */
  def collect[S](pf: PartialFunction[T, S])(implicit executor: ExecutionContext): Future[S] = {
    val p = Promise[S]()

    onComplete {
      case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, S]]
      case Right(v) =>
        try {
          if (pf.isDefinedAt(v)) p success pf(v)
          else p failure new NoSuchElementException("Future.collect partial function is not defined at: " + v)
        } catch {
          case NonFatal(t) => p failure t
        }
    }(executor)

    p.future
  }

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
  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] = {
    val p = Promise[U]()

    onComplete {
      case Left(t) if pf isDefinedAt t =>
        try { p success pf(t) }
        catch {
          case NonFatal(t) => p failure t
        }
      case otherwise => p complete otherwise
    }(executor)

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
   *  val f = future { Int.MaxValue }
   *  future (6 / 0) recoverWith { case e: ArithmeticException => f } // result: Int.MaxValue
   *  }}}
   */
  def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] = {
    val p = Promise[U]()

    onComplete {
      case Left(t) if pf isDefinedAt t =>
        try {
          p completeWith pf(t)
        } catch {
          case NonFatal(t) => p failure t
        }
      case otherwise => p complete otherwise
    }(executor)

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
    val p = Promise[(T, U)]()
    
    this onComplete {
      case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, (T, U)]]
      case Right(r) =>
        that onSuccess {
          case r2 => p success ((r, r2))
        }
        that onFailure {
          case f => p failure f
        }
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
   *  val f = future { sys.error("failed") }
   *  val g = future { 5 }
   *  val h = f fallbackTo g
   *  await(h, 0) // evaluates to 5
   *  }}}
   */
  def fallbackTo[U >: T](that: Future[U]): Future[U] = {
    val p = Promise[U]()
    onComplete {
      case r @ Right(_) ⇒ p complete r
      case _            ⇒ p completeWith that
    }
    p.future
  }

  /** Creates a new `Future[S]` which is completed with this `Future`'s result if
   *  that conforms to `S`'s erased type or a `ClassCastException` otherwise.
   */
  def mapTo[S](implicit tag: ClassTag[S]): Future[S] = {
    def boxedType(c: Class[_]): Class[_] = {
      if (c.isPrimitive) Future.toBoxed(c) else c
    }

    val p = Promise[S]()

    onComplete {
      case l: Left[_, _] => p complete l.asInstanceOf[Left[Throwable, S]]
      case Right(t) =>
        p complete (try {
          Right(boxedType(tag.erasure).cast(t).asInstanceOf[S])
        } catch {
          case e: ClassCastException => Left(e)
        })
    }

    p.future
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
   *  val f = future { 5 }
   *  f andThen {
   *    case r => sys.error("runtime exception")
   *  } andThen {
   *    case Failure(t) => println(t)
   *    case Success(v) => println(v)
   *  }
   *  }}}
   */
  def andThen[U](pf: PartialFunction[Either[Throwable, T], U])(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()

    onComplete {
      case r => try if (pf isDefinedAt r) pf(r) finally p complete r
    }(executor)

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
   *  val h = f either g
   *  await(h, 0) // evaluates to either 5 or throws a runtime exception
   *  }}}
   */
  def either[U >: T](that: Future[U]): Future[U] = {
    val p = Promise[U]()
    val completePromise: PartialFunction[Either[Throwable, U], _] = { case result => p tryComplete result }

    this onComplete completePromise
    that onComplete completePromise

    p.future
  }

}



/** Future companion object.
 *
 *  @define nonDeterministic
 *  Note: using this method yields nondeterministic dataflow programs.
 */
object Future {
  
  import java.{ lang => jl }
  
  private[concurrent] val toBoxed = Map[Class[_], Class[_]](
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
  
  /** Starts an asynchronous computation and returns a `Future` object with the result of that computation.
  *
  *  The result becomes available once the asynchronous computation is completed.
  *
  *  @tparam T       the type of the result
  *  @param body     the asychronous computation
  *  @param execctx  the execution context on which the future is run
  *  @return         the `Future` holding the result of the computation
  */
  def apply[T](body: =>T)(implicit execctx: ExecutionContext): Future[T] = impl.Future(body)

  import scala.collection.mutable.Builder
  import scala.collection.generic.CanBuildFrom

  /** Simple version of `Futures.traverse`. Transforms a `TraversableOnce[Future[A]]` into a `Future[TraversableOnce[A]]`.
   *  Useful for reducing many `Future`s into a single `Future`.
   */
  def sequence[A, M[_] <: TraversableOnce[_]](in: M[Future[A]])(implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]], executor: ExecutionContext): Future[M[A]] = {
    in.foldLeft(Promise.successful(cbf(in)).future) {
      (fr, fa) => for (r <- fr; a <- fa.asInstanceOf[Future[A]]) yield (r += a)
    } map (_.result)
  }

  /** Returns a `Future` to the result of the first future in the list that is completed.
   */
  def firstCompletedOf[T](futures: TraversableOnce[Future[T]])(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()

    val completeFirst: Either[Throwable, T] => Unit = p tryComplete _
    futures.foreach(_ onComplete completeFirst)

    p.future
  }

  /** Returns a `Future` that will hold the optional result of the first `Future` with a result that matches the predicate.
   */
  def find[T](futurestravonce: TraversableOnce[Future[T]])(predicate: T => Boolean)(implicit executor: ExecutionContext): Future[Option[T]] = {
    val futures = futurestravonce.toBuffer
    if (futures.isEmpty) Promise.successful[Option[T]](None).future
    else {
      val result = Promise[Option[T]]()
      val ref = new AtomicInteger(futures.size)
      val search: Either[Throwable, T] => Unit = v => try {
        v match {
          case Right(r) => if (predicate(r)) result tryComplete Right(Some(r))
          case _        =>
        }
      } finally {
        if (ref.decrementAndGet == 0) {
          result tryComplete Right(None)
        }
      }

      futures.foreach(_ onComplete search)

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
  def fold[T, R](futures: TraversableOnce[Future[T]])(zero: R)(foldFun: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) Promise.successful(zero).future
    else sequence(futures).map(_.foldLeft(zero)(foldFun))
  }

  /** Initiates a fold over the supplied futures where the fold-zero is the result value of the `Future` that's completed first.
   *
   *  Example:
   *  {{{
   *    val result = Await.result(Futures.reduce(futures)(_ + _), 5 seconds)
   *  }}}
   */
  def reduce[T, R >: T](futures: TraversableOnce[Future[T]])(op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) Promise[R].failure(new NoSuchElementException("reduce attempted on empty collection")).future
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
  def traverse[A, B, M[_] <: TraversableOnce[_]](in: M[A])(fn: A => Future[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]], executor: ExecutionContext): Future[M[B]] =
    in.foldLeft(Promise.successful(cbf(in)).future) { (fr, a) =>
      val fb = fn(a.asInstanceOf[A])
      for (r <- fr; b <- fb) yield (r += b)
    }.map(_.result)

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
  private[concurrent] object InternalCallbackExecutor extends ExecutionContext {
    def execute(runnable: Runnable): Unit =
      runnable.run()
    def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T =
      throw new IllegalStateException("bug in scala.concurrent, called blocking() from internal callback")
    def reportFailure(t: Throwable): Unit =
      throw new IllegalStateException("problem in scala.concurrent internal callback", t)
  }
}



