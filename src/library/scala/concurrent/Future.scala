/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.language.higherKinds

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


/** A `Future` represents a value which may or may not *currently* be available,
 *  but will be available at some point, or an exception if that value could not be made available.
 *
 *  Asynchronous computations that yield futures are created with the `Future.apply` call and are computed using a supplied `ExecutionContext`,
 * which can be backed by a Thread pool.
 *
 *  {{{
 *  import ExecutionContext.Implicits.global
 *  val s = "Hello"
 *  val f: Future[String] = Future {
 *    s + " future!"
 *  }
 *  f foreach {
 *    msg => println(msg)
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
 *  @define swallowsExceptions
 *  Since this method executes asynchronously and does not produce a return value,
 *  any non-fatal exceptions thrown will be reported to the `ExecutionContext`.
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
 * Completion of the Future must *happen-before* the invocation of the callback.
 */
trait Future[+T] extends Awaitable[T] {
  import Future.{ InternalCallbackExecutor => internalExecutor }

  /* Callbacks */

  /** When this future is completed successfully (i.e., with a value),
   *  apply the provided partial function to the value if the partial function
   *  is defined at that value.
   *
   *  If the future has already been completed with a value,
   *  this will either be applied immediately or be scheduled asynchronously.
   *
   *  Note that the returned value of `pf` will be discarded.
   *
   *  $swallowsExceptions
   *  $multipleCallbacks
   *  $callbackInContext
   *
   * @group Callbacks
   */
  @deprecated("use `foreach` or `onComplete` instead (keep in mind that they take total rather than partial functions)", "2.12.0")
  def onSuccess[U](pf: PartialFunction[T, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Success(v) =>
      pf.applyOrElse[T, Any](v, Predef.identity[T]) // Exploiting the cached function to avoid MatchError
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
   *  Note that the returned value of `pf` will be discarded.
   *
   *  $swallowsExceptions
   *  $multipleCallbacks
   *  $callbackInContext
   *
   * @group Callbacks
   */
  @deprecated("use `onComplete` or `failed.foreach` instead (keep in mind that they take total rather than partial functions)", "2.12.0")
  def onFailure[U](@deprecatedName('callback) pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Failure(t) =>
      pf.applyOrElse[Throwable, Any](t, Predef.identity[Throwable]) // Exploiting the cached function to avoid MatchError
    case _ =>
  }

  /** When this future is completed, either through an exception, or a value,
   *  apply the provided function.
   *
   *  If the future has already been completed,
   *  this will either be applied immediately or be scheduled asynchronously.
   *
   *  Note that the returned value of `f` will be discarded.
   *
   *  $swallowsExceptions
   *  $multipleCallbacks
   *  $callbackInContext
   *
   * @tparam U    only used to accept any return type of the given callback function
   * @param f     the function to be executed when this `Future` completes
   * @group Callbacks
   */
  def onComplete[U](@deprecatedName('func) f: Try[T] => U)(implicit executor: ExecutionContext): Unit


  /* Miscellaneous */

  /** Returns whether the future had already been completed with
   *  a value or an exception.
   *
   *  $nonDeterministic
   *
   *  @return    `true` if the future was completed, `false` otherwise
   * @group Polling
   */
  def isCompleted: Boolean

  /** The current value of this `Future`.
   *
   *  $nonDeterministic
   *
   *  If the future was not completed the returned value will be `None`.
   *  If the future was completed the value will be `Some(Success(t))`
   *  if it contained a valid result, or `Some(Failure(error))` if it contained
   *  an exception.
   *
   * @return    `None` if the `Future` wasn't completed, `Some` if it was.
   * @group Polling
   */
  def value: Option[Try[T]]


  /* Projections */

  /** The returned `Future` will be successfully completed with the `Throwable` of the original `Future`
   *  if the original `Future` fails.
   *
   *  If the original `Future` is successful, the returned `Future` is failed with a `NoSuchElementException`.
   *
   * @return a failed projection of this `Future`.
   * @group Transformations
   */
  def failed: Future[Throwable] =
    transform({
      case Failure(t) => Success(t)
      case Success(v) => Failure(new NoSuchElementException("Future.failed not completed with a throwable."))
    })(internalExecutor)


  /* Monadic operations */

  /** Asynchronously processes the value in the future once the value becomes available.
   *
   *  WARNING: Will not be called if this future is never completed or if it is completed with a failure.
   *
   *  $swallowsExceptions
   *
   * @tparam U     only used to accept any return type of the given callback function
   * @param f      the function which will be executed if this `Future` completes with a result,
   *               the return value of `f` will be discarded.
   * @group Callbacks
   */
  def foreach[U](f: T => U)(implicit executor: ExecutionContext): Unit = onComplete { _ foreach f }

  /** Creates a new future by applying the 's' function to the successful result of
   *  this future, or the 'f' function to the failed result. If there is any non-fatal
   *  exception thrown when 's' or 'f' is applied, that exception will be propagated
   *  to the resulting future.
   *
   * @tparam S  the type of the returned `Future`
   * @param  s  function that transforms a successful result of the receiver into a successful result of the returned future
   * @param  f  function that transforms a failure of the receiver into a failure of the returned future
   * @return    a `Future` that will be completed with the transformed value
   * @group Transformations
   */
  def transform[S](s: T => S, f: Throwable => Throwable)(implicit executor: ExecutionContext): Future[S] =
    transform {
      case Success(r) => Try(s(r))
      case Failure(t) => Try(throw f(t)) // will throw fatal errors!
    }

  /** Creates a new Future by applying the specified function to the result
   * of this Future. If there is any non-fatal exception thrown when 'f'
   * is applied then that exception will be propagated to the resulting future.
   *
   * @tparam S  the type of the returned `Future`
   * @param  f  function that transforms the result of this future
   * @return    a `Future` that will be completed with the transformed value
   * @group Transformations
   */
  def transform[S](f: Try[T] => Try[S])(implicit executor: ExecutionContext): Future[S]

  /** Creates a new Future by applying the specified function, which produces a Future, to the result
   * of this Future. If there is any non-fatal exception thrown when 'f'
   * is applied then that exception will be propagated to the resulting future.
   *
   * @tparam S  the type of the returned `Future`
   * @param  f  function that transforms the result of this future
   * @return    a `Future` that will be completed with the transformed value
   * @group Transformations
   */
  def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): Future[S]


  /** Creates a new future by applying a function to the successful result of
   *  this future. If this future is completed with an exception then the new
   *  future will also contain this exception.
   *
   *  Example:
   *
   *  {{{
   *  val f = Future { "The future" }
   *  val g = f map { x: String => x + " is now!" }
   *  }}}
   *
   *  Note that a for comprehension involving a `Future`
   *  may expand to include a call to `map` and or `flatMap`
   *  and `withFilter`.  See [[scala.concurrent.Future#flatMap]] for an example of such a comprehension.
   *
   *
   * @tparam S  the type of the returned `Future`
   * @param f   the function which will be applied to the successful result of this `Future`
   * @return    a `Future` which will be completed with the result of the application of the function
   * @group Transformations
   */
  def map[S](f: T => S)(implicit executor: ExecutionContext): Future[S] = transform(_ map f)

  /** Creates a new future by applying a function to the successful result of
   *  this future, and returns the result of the function as the new future.
   *  If this future is completed with an exception then the new future will
   *  also contain this exception.
   *
   *  $forComprehensionExamples
   *
   * @tparam S  the type of the returned `Future`
   * @param f   the function which will be applied to the successful result of this `Future`
   * @return    a `Future` which will be completed with the result of the application of the function
   * @group Transformations
   */
  def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = transformWith {
    case Success(s) => f(s)
    case Failure(_) => this.asInstanceOf[Future[S]]
  }

  /** Creates a new future with one level of nesting flattened, this method is equivalent
   *  to `flatMap(identity)`.
   *
   * @tparam S  the type of the returned `Future`
   * @group Transformations
   */
  def flatten[S](implicit ev: T <:< Future[S]): Future[S] = flatMap(ev)(internalExecutor)

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
   *  g foreach println // Eventually prints 5
   *  Await.result(h, Duration.Zero) // throw a NoSuchElementException
   *  }}}
   *
   * @param p   the predicate to apply to the successful result of this `Future`
   * @return    a `Future` which will hold the successful result of this `Future` if it matches the predicate or a `NoSuchElementException`
   * @group Transformations
   */
  def filter(@deprecatedName('pred) p: T => Boolean)(implicit executor: ExecutionContext): Future[T] =
    map { r => if (p(r)) r else throw new NoSuchElementException("Future.filter predicate is not satisfied") }

  /** Used by for-comprehensions.
   * @group Transformations
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
   *  g foreach println // Eventually prints 5
   *  Await.result(h, Duration.Zero) // throw a NoSuchElementException
   *  }}}
   *
   * @tparam S    the type of the returned `Future`
   * @param pf    the `PartialFunction` to apply to the successful result of this `Future`
   * @return      a `Future` holding the result of application of the `PartialFunction` or a `NoSuchElementException`
   * @group Transformations
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
   *
   * @tparam U    the type of the returned `Future`
   * @param pf    the `PartialFunction` to apply if this `Future` fails
   * @return      a `Future` with the successful value of this `Future` or the result of the `PartialFunction`
   * @group Transformations
   */
  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] =
    transform { _ recover pf }

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
   *
   * @tparam U    the type of the returned `Future`
   * @param pf    the `PartialFunction` to apply if this `Future` fails
   * @return      a `Future` with the successful value of this `Future` or the outcome of the `Future` returned by the `PartialFunction`
   * @group Transformations
   */
  def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] =
    transformWith {
      case Failure(t) => pf.applyOrElse(t, (_: Throwable) => this)
      case Success(_) => this
    }

  /** Zips the values of `this` and `that` future, and creates
   *  a new future holding the tuple of their results.
   *
   *  If `this` future fails, the resulting future is failed
   *  with the throwable stored in `this`.
   *  Otherwise, if `that` future fails, the resulting future is failed
   *  with the throwable stored in `that`.
   *
   * @tparam U      the type of the other `Future`
   * @param that    the other `Future`
   * @return        a `Future` with the results of both futures or the failure of the first of them that failed
   * @group Transformations
   */
  def zip[U](that: Future[U]): Future[(T, U)] = {
    implicit val ec = internalExecutor
    flatMap { r1 => that.map(r2 => (r1, r2)) }
  }

  /** Zips the values of `this` and `that` future using a function `f`,
   *  and creates a new future holding the result.
   *
   *  If `this` future fails, the resulting future is failed
   *  with the throwable stored in `this`.
   *  Otherwise, if `that` future fails, the resulting future is failed
   *  with the throwable stored in `that`.
   *  If the application of `f` throws a throwable, the resulting future
   *  is failed with that throwable if it is non-fatal.
   *
   * @tparam U      the type of the other `Future`
   * @tparam R      the type of the resulting `Future`
   * @param that    the other `Future`
   * @param f       the function to apply to the results of `this` and `that`
   * @return        a `Future` with the result of the application of `f` to the results of `this` and `that`
   * @group Transformations
   */
  def zipWith[U, R](that: Future[U])(f: (T, U) => R)(implicit executor: ExecutionContext): Future[R] =
    flatMap(r1 => that.map(r2 => f(r1, r2)))(internalExecutor)

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
   *  h foreach println // Eventually prints 5
   *  }}}
   *
   * @tparam U     the type of the other `Future` and the resulting `Future`
   * @param that   the `Future` whose result we want to use if this `Future` fails.
   * @return       a `Future` with the successful result of this or that `Future` or the failure of this `Future` if both fail
   * @group Transformations
   */
  def fallbackTo[U >: T](that: Future[U]): Future[U] =
    if (this eq that) this
    else {
      implicit val ec = internalExecutor
      recoverWith { case _ => that } recoverWith { case _ => this }
    }

  /** Creates a new `Future[S]` which is completed with this `Future`'s result if
   *  that conforms to `S`'s erased type or a `ClassCastException` otherwise.
   *
   * @tparam S     the type of the returned `Future`
   * @param tag    the `ClassTag` which will be used to cast the result of this `Future`
   * @return       a `Future` holding the casted result of this `Future` or a `ClassCastException` otherwise
   * @group Transformations
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
   *
   * $swallowsExceptions
   *
   * @tparam U     only used to accept any return type of the given `PartialFunction`
   * @param pf     a `PartialFunction` which will be conditionally applied to the outcome of this `Future`
   * @return       a `Future` which will be completed with the exact same outcome as this `Future` but after the `PartialFunction` has been executed.
   * @group Callbacks
   */
  def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): Future[T] =
    transform {
      result =>
        try pf.applyOrElse[Try[T], Any](result, Predef.identity[Try[T]])
        catch { case NonFatal(t) => executor reportFailure t }

        result
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

  /** A Future which is never completed.
   */
  final object never extends Future[Nothing] {

    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
      atMost match {
        case e if e eq Duration.Undefined => throw new IllegalArgumentException("cannot wait for Undefined period")
        case Duration.Inf        => new CountDownLatch(1).await()
        case Duration.MinusInf   => // Drop out
        case f: FiniteDuration   =>
          if (f > Duration.Zero) new CountDownLatch(1).await(f.toNanos, TimeUnit.NANOSECONDS)
      }
      throw new TimeoutException(s"Future timed out after [$atMost]")
    }

    @throws(classOf[Exception])
    override def result(atMost: Duration)(implicit permit: CanAwait): Nothing = {
      ready(atMost)
      throw new TimeoutException(s"Future timed out after [$atMost]")
    }

    override def onSuccess[U](pf: PartialFunction[Nothing, U])(implicit executor: ExecutionContext): Unit = ()
    override def onFailure[U](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = ()
    override def onComplete[U](f: Try[Nothing] => U)(implicit executor: ExecutionContext): Unit = ()
    override def isCompleted: Boolean = false
    override def value: Option[Try[Nothing]] = None
    override def failed: Future[Throwable] = this
    override def foreach[U](f: Nothing => U)(implicit executor: ExecutionContext): Unit = ()
    override def transform[S](s: Nothing => S, f: Throwable => Throwable)(implicit executor: ExecutionContext): Future[S] = this
    override def transform[S](f: Try[Nothing] => Try[S])(implicit executor: ExecutionContext): Future[S] = this
    override def transformWith[S](f: Try[Nothing] => Future[S])(implicit executor: ExecutionContext): Future[S] = this
    override def map[S](f: Nothing => S)(implicit executor: ExecutionContext): Future[S] = this
    override def flatMap[S](f: Nothing => Future[S])(implicit executor: ExecutionContext): Future[S] = this
    override def flatten[S](implicit ev: Nothing <:< Future[S]): Future[S] = this
    override def filter(p: Nothing => Boolean)(implicit executor: ExecutionContext): Future[Nothing] = this
    override def collect[S](pf: PartialFunction[Nothing, S])(implicit executor: ExecutionContext): Future[S] = this
    override def recover[U >: Nothing](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] = this
    override def recoverWith[U >: Nothing](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] = this
    override def zip[U](that: Future[U]): Future[(Nothing, U)] = this
    override def zipWith[U, R](that: Future[U])(f: (Nothing, U) => R)(implicit executor: ExecutionContext): Future[R] = this
    override def fallbackTo[U >: Nothing](that: Future[U]): Future[U] = this
    override def mapTo[S](implicit tag: ClassTag[S]): Future[S] = this
    override def andThen[U](pf: PartialFunction[Try[Nothing], U])(implicit executor: ExecutionContext): Future[Nothing] = this

    override def toString: String = "Future(<never>)"
  }

  /** A Future which is always completed with the Unit value.
   */
  val unit: Future[Unit] = successful(())

  /** Creates an already completed Future with the specified exception.
   *
   *  @tparam T        the type of the value in the future
   *  @param exception the non-null instance of `Throwable`
   *  @return          the newly created `Future` instance
   */
  def failed[T](exception: Throwable): Future[T] = Promise.failed(exception).future

  /** Creates an already completed Future with the specified result.
   *
   *  @tparam T       the type of the value in the future
   *  @param result   the given successful value
   *  @return         the newly created `Future` instance
   */
  def successful[T](result: T): Future[T] = Promise.successful(result).future

  /** Creates an already completed Future with the specified result or exception.
   *
   *  @tparam T       the type of the value in the `Future`
   *  @param result   the result of the returned `Future` instance
   *  @return         the newly created `Future` instance
   */
  def fromTry[T](result: Try[T]): Future[T] = Promise.fromTry(result).future

  /** Starts an asynchronous computation and returns a `Future` instance with the result of that computation.
  *
  *  The following expressions are equivalent:
  *
  *  {{{
  *  val f1 = Future(expr)
  *  val f2 = Future.unit.map(_ => expr)
  *  }}}
  *
  *  The result becomes available once the asynchronous computation is completed.
  *
  *  @tparam T        the type of the result
  *  @param body      the asynchronous computation
  *  @param executor  the execution context on which the future is run
  *  @return          the `Future` holding the result of the computation
  */
  def apply[T](body: =>T)(implicit @deprecatedName('execctx) executor: ExecutionContext): Future[T] =
    unit.map(_ => body)

  /** Simple version of `Future.traverse`. Asynchronously and non-blockingly transforms a `TraversableOnce[Future[A]]`
   *  into a `Future[TraversableOnce[A]]`. Useful for reducing many `Future`s into a single `Future`.
   *
   * @tparam A        the type of the value inside the Futures
   * @tparam M        the type of the `TraversableOnce` of Futures
   * @param in        the `TraversableOnce` of Futures which will be sequenced
   * @return          the `Future` of the `TraversableOnce` of results
   */
  def sequence[A, M[X] <: TraversableOnce[X]](in: M[Future[A]])(implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]], executor: ExecutionContext): Future[M[A]] = {
    in.foldLeft(successful(cbf(in))) {
      (fr, fa) => fr.zipWith(fa)(_ += _)
    }.map(_.result())(InternalCallbackExecutor)
  }

  /** Asynchronously and non-blockingly returns a new `Future` to the result of the first future
   *  in the list that is completed. This means no matter if it is completed as a success or as a failure.
   *
   * @tparam T        the type of the value in the future
   * @param futures   the `TraversableOnce` of Futures in which to find the first completed
   * @return          the `Future` holding the result of the future that is first to be completed
   */
  def firstCompletedOf[T](futures: TraversableOnce[Future[T]])(implicit executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    val completeFirst: Try[T] => Unit = p tryComplete _
    futures foreach { _ onComplete completeFirst }
    p.future
  }

  /** Asynchronously and non-blockingly returns a `Future` that will hold the optional result
   *  of the first `Future` with a result that matches the predicate.
   *
   * @tparam T        the type of the value in the future
   * @param futures   the `TraversableOnce` of Futures to search
   * @param p         the predicate which indicates if it's a match
   * @return          the `Future` holding the optional result of the search
   */
  @deprecated("use the overloaded version of this method that takes a scala.collection.immutable.Iterable instead", "2.12.0")
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


  /** Asynchronously and non-blockingly returns a `Future` that will hold the optional result
   *  of the first `Future` with a result that matches the predicate, failed `Future`s will be ignored.
   *
   * @tparam T        the type of the value in the future
   * @param futures   the `scala.collection.immutable.Iterable` of Futures to search
   * @param p         the predicate which indicates if it's a match
   * @return          the `Future` holding the optional result of the search
   */
  def find[T](futures: scala.collection.immutable.Iterable[Future[T]])(p: T => Boolean)(implicit executor: ExecutionContext): Future[Option[T]] = {
    def searchNext(i: Iterator[Future[T]]): Future[Option[T]] =
      if (!i.hasNext) successful[Option[T]](None)
      else {
        i.next().transformWith {
          case Success(r) if p(r) => successful(Some(r))
          case other => searchNext(i)
        }
      }
    searchNext(futures.iterator)
  }

  /** A non-blocking, asynchronous left fold over the specified futures,
   *  with the start value of the given zero.
   *  The fold is performed asynchronously in left-to-right order as the futures become completed.
   *  The result will be the first failure of any of the futures, or any failure in the actual fold,
   *  or the result of the fold.
   *
   *  Example:
   *  {{{
   *    val futureSum = Future.foldLeft(futures)(0)(_ + _)
   *  }}}
   *
   * @tparam T       the type of the value of the input Futures
   * @tparam R       the type of the value of the returned `Future`
   * @param futures  the `scala.collection.immutable.Iterable` of Futures to be folded
   * @param zero     the start value of the fold
   * @param op       the fold operation to be applied to the zero and futures
   * @return         the `Future` holding the result of the fold
   */
  def foldLeft[T, R](futures: scala.collection.immutable.Iterable[Future[T]])(zero: R)(op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] =
    foldNext(futures.iterator, zero, op)

  private[this] def foldNext[T, R](i: Iterator[Future[T]], prevValue: R, op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] =
    if (!i.hasNext) successful(prevValue)
    else i.next().flatMap { value => foldNext(i, op(prevValue, value), op) }

  /** A non-blocking, asynchronous fold over the specified futures, with the start value of the given zero.
   *  The fold is performed on the thread where the last future is completed,
   *  the result will be the first failure of any of the futures, or any failure in the actual fold,
   *  or the result of the fold.
   *
   *  Example:
   *  {{{
   *    val futureSum = Future.fold(futures)(0)(_ + _)
   *  }}}
   *
   * @tparam T       the type of the value of the input Futures
   * @tparam R       the type of the value of the returned `Future`
   * @param futures  the `TraversableOnce` of Futures to be folded
   * @param zero     the start value of the fold
   * @param op       the fold operation to be applied to the zero and futures
   * @return         the `Future` holding the result of the fold
   */
  @deprecated("use Future.foldLeft instead", "2.12.0")
  def fold[T, R](futures: TraversableOnce[Future[T]])(zero: R)(@deprecatedName('foldFun) op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) successful(zero)
    else sequence(futures).map(_.foldLeft(zero)(op))
  }

  /** Initiates a non-blocking, asynchronous, fold over the supplied futures
   *  where the fold-zero is the result value of the `Future` that's completed first.
   *
   *  Example:
   *  {{{
   *    val futureSum = Future.reduce(futures)(_ + _)
   *  }}}
   * @tparam T       the type of the value of the input Futures
   * @tparam R       the type of the value of the returned `Future`
   * @param futures  the `TraversableOnce` of Futures to be reduced
   * @param op       the reduce operation which is applied to the results of the futures
   * @return         the `Future` holding the result of the reduce
   */
  @deprecated("use Future.reduceLeft instead", "2.12.0")
  def reduce[T, R >: T](futures: TraversableOnce[Future[T]])(op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    if (futures.isEmpty) failed(new NoSuchElementException("reduce attempted on empty collection"))
    else sequence(futures).map(_ reduceLeft op)
  }

  /** Initiates a non-blocking, asynchronous, left reduction over the supplied futures
   *  where the zero is the result value of the first `Future`.
   *
   *  Example:
   *  {{{
   *    val futureSum = Future.reduceLeft(futures)(_ + _)
   *  }}}
   * @tparam T       the type of the value of the input Futures
   * @tparam R       the type of the value of the returned `Future`
   * @param futures  the `scala.collection.immutable.Iterable` of Futures to be reduced
   * @param op       the reduce operation which is applied to the results of the futures
   * @return         the `Future` holding the result of the reduce
   */
  def reduceLeft[T, R >: T](futures: scala.collection.immutable.Iterable[Future[T]])(op: (R, T) => R)(implicit executor: ExecutionContext): Future[R] = {
    val i = futures.iterator
    if (!i.hasNext) failed(new NoSuchElementException("reduceLeft attempted on empty collection"))
    else i.next() flatMap { v => foldNext(i, v, op) }
  }

  /** Asynchronously and non-blockingly transforms a `TraversableOnce[A]` into a `Future[TraversableOnce[B]]`
   *  using the provided function `A => Future[B]`.
   *  This is useful for performing a parallel map. For example, to apply a function to all items of a list
   *  in parallel:
   *
   *  {{{
   *    val myFutureList = Future.traverse(myList)(x => Future(myFunc(x)))
   *  }}}
   * @tparam A        the type of the value inside the Futures in the `TraversableOnce`
   * @tparam B        the type of the value of the returned `Future`
   * @tparam M        the type of the `TraversableOnce` of Futures
   * @param in        the `TraversableOnce` of Futures which will be sequenced
   * @param fn        the function to apply to the `TraversableOnce` of Futures to produce the results
   * @return          the `Future` of the `TraversableOnce` of results
   */
  def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Future[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]], executor: ExecutionContext): Future[M[B]] =
    in.foldLeft(successful(cbf(in))) {
      (fr, a) => fr.zipWith(fn(a))(_ += _)
    }.map(_.result())(InternalCallbackExecutor)


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

