package scala.util.control

/** Classes representing the components of exception handling.
 *  Each class is independently composable.  Some common uses:
 *
 *  <pre>
 *  <b>import</b> scala.util.control.Exception._
 *  <b>import</b> java.net._
 *
 *  <b>val</b> s = "http://www.scala-lang.org/"
 *  <b>val</b> x1 = catching(classOf[MalformedURLException]) opt new URL(s)
 *  <b>val</b> x2 = catching(classOf[MalformedURLException], classOf[NullPointerException]) either new URL(s)
 *  </pre>
 *  @author Paul Phillips
 */

import java.lang.reflect.InvocationTargetException

object Exception
{
  // We get lots of crashes using this, so for now we just use Class[_]
  // type ExClass = Class[_ <: Throwable]

  type Catcher[+T] = PartialFunction[Throwable, T]
  type ExceptionCatcher[+T] = PartialFunction[Exception, T]

  // due to the magic of contravariance, Throwable => T is a subtype of
  // Exception => T, not the other way around.  So we manually construct
  // a Throwable => T and simply rethrow the non-Exceptions.
  implicit def fromExceptionCatcher[T](pf: ExceptionCatcher[T]): Catcher[T] = {
    new PartialFunction[Throwable, T] {
      def isDefinedAt(x: Throwable) = x match {
        case e: Exception if pf.isDefinedAt(e)  => true
        case _                                  => false
      }
      def apply(x: Throwable) = x match {
        case e: Exception if pf.isDefinedAt(e)  => pf(e)
        case e                                  => throw e
      }
    }
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
    def invoke(): Unit = { body }
  }

  /** A container class for catch/finally logic. */
  class Catch[+T] private[Exception](
    private[Exception] val pf: Catcher[T],
    private[Exception] val fin: Option[Finally])
  extends Described
  {
    def this(pf: Catcher[T]) = this(pf, None)
    protected val name = "Catch"

    /** Create a new Catch with additional exception handling logic. */
    def or[U >: T](pf2: Catcher[U]): Catch[U] = new Catch(pf orElse pf2, fin)
    def or[U >: T](other: Catch[U]): Catch[U] = or(other.pf)

    /** Apply this catch logic to the supplied body. */
    def apply[U >: T](body: => U): U = {
      try     { body }
      catch   { case e if pf.isDefinedAt(e) => pf(e) }
      finally { fin.map(_.invoke()) }
    }

    /* Create an empty Try container with this Catch and the supplied Finally */
    def andFinally(body: => Unit): Catch[T] = fin match {
      case None     => new Catch(pf, Some(new Finally(body)))
      case Some(f)  => new Catch(pf, Some(f and body))
    }

    /** Apply this catch logic to the supplied body, mapping the result
     *  into Option[T] - None if any exception was caught, Some(T) otherwise.
     */
    def opt[U >: T](body: => U): Option[U] = toOption(Some(body))

    /** Apply this catch logic to the supplied body, mapping the result
     *  into Either[Throwable, T] - Left(exception) if an exception was caught,
     *  Right(T) otherwise.
     */
    def either[U >: T](body: => U): Either[Throwable, U] = toEither(Right(body))

    /** Create a new Catch with the same isDefinedAt logic as this one,
      * but with the supplied apply method replacing the current one. */
    def withApply[U](f: (Throwable) => U): Catch[U] = {
      val pf2 = new PartialFunction[Throwable, U] {
        def isDefinedAt(x: Throwable) = pf isDefinedAt x
        def apply(x: Throwable) = f(x)
      }
      new Catch(pf2, fin)
    }

    /** Convenience methods. */
    def toOption: Catch[Option[T]] = withApply(_ => None)
    def toEither: Catch[Either[Throwable, T]] = withApply(Left(_))
  }

  /** A container class for Try logic */
  class Try[+T] private[Exception](body: => T, val catcher: Catch[T]) {
    /** Execute "body" using catch/finally logic "catcher" */
    def apply(): T                    = catcher(body)
    def apply[U >: T](other: => U): U = catcher(other)

    /** As apply, but map caught exceptions to None and success to Some(T) */
    def opt(): Option[T]                      = catcher opt body
    def opt[U >: T](other: => U): Option[U]   = catcher opt other

    /** As apply, but map caught exceptions to Left(ex) and success to Right(x) */
    def either(): Either[Throwable, T]                    = catcher either body
    def either[U >: T](other: => U): Either[Throwable, U] = catcher either other

    /** Create a new Try with the supplied body replacing the current body */
    def tryInstead[U >: T](other: => U) = new Try(other, catcher)

    /** Create a new Try with the supplied logic appended to the existing Catch logic. */
    def or[U >: T](pf: Catcher[U]) = new Try(body, catcher or pf)

    /** Create a new Try with the supplied code appended to the existing Finally. */
    def andFinally(fin: => Unit) = new Try(body, catcher andFinally fin)

    override def toString() = List("Try(<body>)", catcher.toString) mkString " "
  }

  final val nothingCatcher: PartialFunction[Throwable, Nothing] =
    new PartialFunction[Throwable, Nothing] {
      def isDefinedAt(x: Throwable) = false
      def apply(x: Throwable) = throw x
    }

  /** The empty Catch object. */
  final val noCatch: Catch[Nothing] = new Catch(nothingCatcher) withDesc "<nothing>"

  /** Creates a Catch object which will catch any of the supplied exceptions.
    * Since the returned Catch object has no specific logic defined and will simply
    * rethrow the exceptions it catches, you will typically want to call "opt" or
    * "either" on the return value, or assign custom logic by calling "withApply".
    */
  def catching[T](exceptions: Class[_]*): Catch[T] =
    new Catch(pfFromExceptions(exceptions : _*)) withDesc exceptions.map(_.getName).mkString(", ")
  def catching[T](c: Catcher[T]): Catch[T] = new Catch(c)

  /** Creates a Catch object which catches and ignores any of the supplied exceptions. */
  def ignoring(exceptions: Class[_]*): Catch[Unit] =
    catching(exceptions: _*) withApply (_ => ())

  /** Creates a Catch object which maps all the supplied exceptions to 'None'. */
  def failing[T](exceptions: Class[_]*): Catch[Option[T]] =
    catching(exceptions: _*) withApply (_ => None)

  /** Returns a partially constructed Catch object, which you must give
    * an exception handler function as an argument to "by".  Example:
    * handling(ex1, ex2) by (_.printStackTrace)
    */
  class By[T,R](f: T => R) {
    def by(x: T): R = f(x)
  }
  def handling[T](exceptions: Class[_]*) = {
    def fun(f: Throwable => T) = catching(exceptions: _*) withApply f
    new By[Throwable => T, Catch[T]](fun _)
  }

  /** Returns a Catch object with no catch logic and the argument as Finally. */
  def ultimately[T](body: => Unit): Catch[T] = noCatch andFinally body

  /** Experimental */
  def saving[A](oldVal: A, newVal: A, setter: (A) => Unit): Catch[Nothing] = {
    new Catch(nothingCatcher, Some(new Finally(setter(oldVal)))) {
      override def apply[U](body: => U): U = {
        setter(newVal)
        super.apply(body)
      }
    }
  }

  /** Creates a Catch object which unwraps any of the supplied exceptions. */
  def unwrapping[T](exceptions: Class[_]*): Catch[T] = {
    def unwrap(x: Throwable): Throwable =
      if (wouldMatch(x, exceptions) && x.getCause != null) unwrap(x.getCause)
      else x

    catching(exceptions: _*) withApply (x => throw unwrap(x))
  }

  /** Private **/
  private def wouldMatch(x: Throwable, classes: collection.Sequence[Class[_]]): Boolean =
    classes exists (_ isAssignableFrom x.getClass)

  private def pfFromExceptions(exceptions: Class[_]*) =
    new PartialFunction[Throwable, Nothing] {
      def apply(x: Throwable) = throw x
      def isDefinedAt(x: Throwable) = wouldMatch(x, exceptions)
    }
}
