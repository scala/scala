/*/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.akka






sealed trait Future[+T] extends scala.concurrent.Future with Awaitable[T] {
  
  implicit def executor: ExecutionContext
  
  /**
   * For use only within a Future.flow block or another compatible Delimited Continuations reset block.
   *
   * Returns the result of this Future without blocking, by suspending execution and storing it as a
   * continuation until the result is available.
   */
  def apply(): T @cps[Future[Any]] = shift(this flatMap (_: T ⇒ Future[Any]))
  
  /**
   * Tests whether this Future has been completed.
   */
  final def isCompleted: Boolean = value.isDefined
  
  /**
   * The contained value of this Future. Before this Future is completed
   * the value will be None. After completion the value will be Some(Right(t))
   * if it contains a valid result, or Some(Left(error)) if it contains
   * an exception.
   */
  def value: Option[Either[Throwable, T]]
  
  def onComplete(func: Either[Throwable, T] => Unit): this.type
  
  /**
   * Creates a Future that will be the result of the first completed Future of this and the Future that was passed into this.
   * This is semantically the same as: Future.firstCompletedOf(Seq(this, that))
   */
  //FIXME implement as the result of any of the Futures, or if both failed, the first failure
  def orElse[A >: T](that: Future[A]): Future[A] = Future.firstCompletedOf(List(this, that)) //TODO Optimize
  
  final def recover[A >: T](pf: PartialFunction[Throwable, A]): Future[A] = {
    val future = Promise[A]()
    onComplete {
      case Left(e) if pf isDefinedAt e ⇒ future.complete(try { Right(pf(e)) } catch { case x: Exception ⇒ Left(x) })
      case otherwise                   ⇒ future complete otherwise
    }
    future
  }
  
  /**
   * Creates a new Future by applying a function to the successful result of
   * this Future. If this Future is completed with an exception then the new
   * Future will also contain this exception.
   * Example:
   * <pre>
   * val future1 = for {
   *   a: Int    <- actor ? "Hello" // returns 5
   *   b: String <- actor ? a       // returns "10"
   *   c: String <- actor ? 7       // returns "14"
   * } yield b + "-" + c
   * </pre>
   */
  final def map[A](f: T ⇒ A): Future[A] = {
    val future = Promise[A]()
    onComplete {
      case l: Left[_, _] ⇒ future complete l.asInstanceOf[Either[Throwable, A]]
      case Right(res) ⇒
        future complete (try {
          Right(f(res))
        } catch {
          case e ⇒
            logError("Future.map", e)
            Left(e)
        })
    }
    future
  }

  /**
   * Creates a new Future[A] which is completed with this Future's result if
   * that conforms to A's erased type or a ClassCastException otherwise.
   */
  final def mapTo[A](implicit m: Manifest[A]): Future[A] = {
    val fa = Promise[A]()
    onComplete {
      case l: Left[_, _] ⇒ fa complete l.asInstanceOf[Either[Throwable, A]]
      case Right(t) ⇒
        fa complete (try {
          Right(BoxedType(m.erasure).cast(t).asInstanceOf[A])
        } catch {
          case e: ClassCastException ⇒ Left(e)
        })
    }
    fa
  }

  /**
   * Creates a new Future by applying a function to the successful result of
   * this Future, and returns the result of the function as the new Future.
   * If this Future is completed with an exception then the new Future will
   * also contain this exception.
   * Example:
   * <pre>
   * val future1 = for {
   *   a: Int    <- actor ? "Hello" // returns 5
   *   b: String <- actor ? a       // returns "10"
   *   c: String <- actor ? 7       // returns "14"
   * } yield b + "-" + c
   * </pre>
   */
  final def flatMap[A](f: T ⇒ Future[A]): Future[A] = {
    val p = Promise[A]()

    onComplete {
      case l: Left[_, _] ⇒ p complete l.asInstanceOf[Either[Throwable, A]]
      case Right(r) ⇒
        try {
          p completeWith f(r)
        } catch {
          case e ⇒
            p complete Left(e)
            logError("Future.flatMap", e)
        }
    }
    p
  }

  /**
   * Same as onSuccess { case r => f(r) } but is also used in for-comprehensions
   */
  final def foreach(f: T ⇒ Unit): Unit = onComplete {
    case Right(r) ⇒ f(r)
    case _        ⇒
  }

  /**
   * Used by for-comprehensions
   */
  final def withFilter(p: T ⇒ Boolean) = new FutureWithFilter[T](this, p)

  final class FutureWithFilter[+A](self: Future[A], p: A ⇒ Boolean) {
    def foreach(f: A ⇒ Unit): Unit = self filter p foreach f
    def map[B](f: A ⇒ B): Future[B] = self filter p map f
    def flatMap[B](f: A ⇒ Future[B]): Future[B] = self filter p flatMap f
    def withFilter(q: A ⇒ Boolean): FutureWithFilter[A] = new FutureWithFilter[A](self, x ⇒ p(x) && q(x))
  }

  /**
   * Returns a new Future that will hold the successful result of this Future if it matches
   * the given predicate, if it doesn't match, the resulting Future will be a failed Future
   * with a MatchError, of if this Future fails, that failure will be propagated to the returned Future
   */
  final def filter(pred: T ⇒ Boolean): Future[T] = {
    val p = Promise[T]()
    onComplete {
      case l: Left[_, _] ⇒ p complete l.asInstanceOf[Either[Throwable, T]]
      case r @ Right(res) ⇒ p complete (try {
        if (pred(res)) r else Left(new MatchError(res))
      } catch {
        case e ⇒
          logError("Future.filter", e)
          Left(e)
      })
    }
    p
  }

  protected def logError(msg: String, problem: Throwable): Unit = {
    executor match {
      case m: MessageDispatcher ⇒ m.prerequisites.eventStream.publish(Error(problem, msg, problem.getMessage))
      case other                ⇒ problem.printStackTrace()
    }
  }
}



*/
