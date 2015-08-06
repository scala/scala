/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util
package control

import scala.collection.immutable.List
import scala.reflect.{ ClassTag, classTag }
import java.lang.reflect.InvocationTargetException
import scala.language.implicitConversions


/** Classes representing the components of exception handling.
 *  Each class is independently composable.  Some example usages:
 *  {{{
 *  import scala.util.control.Exception._
 *  import java.net._
 *
 *  val s = "http://www.scala-lang.org/"
 *  val x1 = catching(classOf[MalformedURLException]) opt new URL(s)
 *  val x2 = catching(classOf[MalformedURLException], classOf[NullPointerException]) either new URL(s)
 *  }}}
 *
 *  This class differs from `scala.util.Try` in that it focuses on composing exception handlers rather than
 *  composing behavior.   All behavior should be composed first and fed to a `Catch` object using one of the
 *  `opt` or `either` methods.
 *
 *  @author Paul Phillips
 */

object Exception {
  type Catcher[+T] = PartialFunction[Throwable, T]

  def mkCatcher[Ex <: Throwable: ClassTag, T](isDef: Ex => Boolean, f: Ex => T) = new Catcher[T] {
    private def downcast(x: Throwable): Option[Ex] =
      if (classTag[Ex].runtimeClass.isAssignableFrom(x.getClass)) Some(x.asInstanceOf[Ex])
      else None

    def isDefinedAt(x: Throwable) = downcast(x) exists isDef
    def apply(x: Throwable): T = f(downcast(x).get)
  }

  def mkThrowableCatcher[T](isDef: Throwable => Boolean, f: Throwable => T) = mkCatcher(isDef, f)

  implicit def throwableSubtypeToCatcher[Ex <: Throwable: ClassTag, T](pf: PartialFunction[Ex, T]) =
    mkCatcher(pf.isDefinedAt _, pf.apply _)

  /** !!! Not at all sure of every factor which goes into this,
   *  and/or whether we need multiple standard variations.
   */
  def shouldRethrow(x: Throwable): Boolean = x match {
    case _: ControlThrowable      => true
    case _: InterruptedException  => true
    // case _: java.lang.Error       => true ?
    case _                        => false
  }

  trait Described {
    protected val name: String
    private var _desc: String = ""
    def desc = _desc
    def withDesc(s: String): this.type = {
      _desc = s
      this
    }
    override def toString() = name + "(" + desc + ")"
  }

  /** A container class for finally code. */
  class Finally private[Exception](body: => Unit) extends Described {
    protected val name = "Finally"

    def and(other: => Unit): Finally = new Finally({ body ; other })
    def invoke() { body }
  }

  /** A container class for catch/finally logic.
   *
   *  Pass a different value for rethrow if you want to probably
   *  unwisely allow catching control exceptions and other throwables
   *  which the rest of the world may expect to get through.
   */
  class Catch[+T](
    val pf: Catcher[T],
    val fin: Option[Finally] = None,
    val rethrow: Throwable => Boolean = shouldRethrow)
  extends Described {

    protected val name = "Catch"

    /** Create a new Catch with additional exception handling logic. */
    def or[U >: T](pf2: Catcher[U]): Catch[U] = new Catch(pf orElse pf2, fin, rethrow)
    def or[U >: T](other: Catch[U]): Catch[U] = or(other.pf)

    /** Apply this catch logic to the supplied body. */
    def apply[U >: T](body: => U): U =
      try body
      catch {
        case x if rethrow(x)        => throw x
        case x if pf isDefinedAt x  => pf(x)
      }
      finally fin foreach (_.invoke())

    /* Create an empty Try container with this Catch and the supplied `Finally`. */
    def andFinally(body: => Unit): Catch[T] = fin match {
      case None     => new Catch(pf, Some(new Finally(body)), rethrow)
      case Some(f)  => new Catch(pf, Some(f and body), rethrow)
    }

    /** Apply this catch logic to the supplied body, mapping the result
     *  into `Option[T]` - `None` if any exception was caught, `Some(T)` otherwise.
     */
    def opt[U >: T](body: => U): Option[U] = toOption(Some(body))

    /** Apply this catch logic to the supplied body, mapping the result
     *  into Either[Throwable, T] - Left(exception) if an exception was caught,
     *  Right(T) otherwise.
     */
    def either[U >: T](body: => U): Either[Throwable, U] = toEither(Right(body))

    /** Apply this catch logic to the supplied body, mapping the result
     * into Try[T] - Failure if an exception was caught, Success(T) otherwise.
     */
    def withTry[U >: T](body: => U): scala.util.Try[U] = toTry(Success(body))

    /** Create a `Catch` object with the same `isDefinedAt` logic as this one,
      * but with the supplied `apply` method replacing the current one. */
    def withApply[U](f: Throwable => U): Catch[U] = {
      val pf2 = new Catcher[U] {
        def isDefinedAt(x: Throwable) = pf isDefinedAt x
        def apply(x: Throwable) = f(x)
      }
      new Catch(pf2, fin, rethrow)
    }

    /** Convenience methods. */
    def toOption: Catch[Option[T]] = withApply(_ => None)
    def toEither: Catch[Either[Throwable, T]] = withApply(Left(_))
    def toTry: Catch[scala.util.Try[T]] = withApply(x => Failure(x))
  }

  final val nothingCatcher: Catcher[Nothing]  = mkThrowableCatcher(_ => false, throw _)
  final def nonFatalCatcher[T]: Catcher[T]    = mkThrowableCatcher({ case NonFatal(_) => true; case _ => false }, throw _)
  final def allCatcher[T]: Catcher[T]         = mkThrowableCatcher(_ => true, throw _)

  /** The empty `Catch` object. */
  final val noCatch: Catch[Nothing] = new Catch(nothingCatcher) withDesc "<nothing>"

  /** A `Catch` object which catches everything. */
  final def allCatch[T]: Catch[T] = new Catch(allCatcher[T]) withDesc "<everything>"

  /** A `Catch` object which catches non-fatal exceptions. */
  final def nonFatalCatch[T]: Catch[T] = new Catch(nonFatalCatcher[T]) withDesc "<non-fatal>"

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Since the returned `Catch` object has no specific logic defined and will simply
   *  rethrow the exceptions it catches, you will typically want to call `opt` or
   *  `either` on the return value, or assign custom logic by calling "withApply".
   *
   *  Note that `Catch` objects automatically rethrow `ControlExceptions` and others
   *  which should only be caught in exceptional circumstances.  If you really want
   *  to catch exactly what you specify, use `catchingPromiscuously` instead.
   */
  def catching[T](exceptions: Class[_]*): Catch[T] =
    new Catch(pfFromExceptions(exceptions : _*)) withDesc (exceptions map (_.getName) mkString ", ")

  def catching[T](c: Catcher[T]): Catch[T] = new Catch(c)

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Unlike "catching" which filters out those in shouldRethrow, this one will
   *  catch whatever you ask of it: `ControlThrowable`, `InterruptedException`,
   *  `OutOfMemoryError`, you name it.
   */
  def catchingPromiscuously[T](exceptions: Class[_]*): Catch[T] = catchingPromiscuously(pfFromExceptions(exceptions : _*))
  def catchingPromiscuously[T](c: Catcher[T]): Catch[T]         = new Catch(c, None, _ => false)

  /** Creates a `Catch` object which catches and ignores any of the supplied exceptions. */
  def ignoring(exceptions: Class[_]*): Catch[Unit] =
    catching(exceptions: _*) withApply (_ => ())

  /** Creates a `Catch` object which maps all the supplied exceptions to `None`. */
  def failing[T](exceptions: Class[_]*): Catch[Option[T]] =
    catching(exceptions: _*) withApply (_ => None)

  /** Creates a `Catch` object which maps all the supplied exceptions to the given value. */
  def failAsValue[T](exceptions: Class[_]*)(value: => T): Catch[T] =
    catching(exceptions: _*) withApply (_ => value)

  /** Returns a partially constructed `Catch` object, which you must give
    * an exception handler function as an argument to `by`.  Example:
    * {{{
    *   handling(ex1, ex2) by (_.printStackTrace)
    * }}}
    */
  class By[T,R](f: T => R) {
    def by(x: T): R = f(x)
  }
  def handling[T](exceptions: Class[_]*) = {
    def fun(f: Throwable => T) = catching(exceptions: _*) withApply f
    new By[Throwable => T, Catch[T]](fun _)
  }

  /** Returns a `Catch` object with no catch logic and the argument as `Finally`. */
  def ultimately[T](body: => Unit): Catch[T] = noCatch andFinally body

  /** Creates a `Catch` object which unwraps any of the supplied exceptions. */
  def unwrapping[T](exceptions: Class[_]*): Catch[T] = {
    def unwrap(x: Throwable): Throwable =
      if (wouldMatch(x, exceptions) && x.getCause != null) unwrap(x.getCause)
      else x

    catching(exceptions: _*) withApply (x => throw unwrap(x))
  }

  /** Private **/
  private def wouldMatch(x: Throwable, classes: scala.collection.Seq[Class[_]]): Boolean =
    classes exists (_ isAssignableFrom x.getClass)

  private def pfFromExceptions(exceptions: Class[_]*): PartialFunction[Throwable, Nothing] =
    { case x if wouldMatch(x, exceptions) => throw x }
}
