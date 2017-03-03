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

import scala.reflect.{ ClassTag, classTag }
import scala.language.implicitConversions

/** Classes representing the components of exception handling.
 *
 *  Each class is independently composable.
 *
 *  This class differs from [[scala.util.Try]] in that it focuses on composing exception handlers rather than
 *  composing behavior.  All behavior should be composed first and fed to a [[Catch]] object using one of the
 *  `opt`, `either` or `withTry` methods. Taken together the classes provide a DSL for composing catch and finally
 *  behaviors.
 *
 *  === Examples ===
 *
 *  Create a `Catch` which handles specified exceptions.
 *  {{{
 *  import scala.util.control.Exception._
 *  import java.net._
 *
 *  val s = "http://www.scala-lang.org/"
 *
 *  // Some(http://www.scala-lang.org/)
 *  val x1: Option[URL] = catching(classOf[MalformedURLException]) opt new URL(s)
 *
 *  // Right(http://www.scala-lang.org/)
 *  val x2: Either[Throwable,URL] =
 *    catching(classOf[MalformedURLException], classOf[NullPointerException]) either new URL(s)
 *
 *  // Success(http://www.scala-lang.org/)
 *  val x3: Try[URL] = catching(classOf[MalformedURLException], classOf[NullPointerException]) withTry new URL(s)
 *
 *  val defaultUrl = new URL("http://example.com")
 *  //  URL(http://example.com) because htt/xx throws MalformedURLException
 *  val x4: URL = failAsValue(classOf[MalformedURLException])(defaultUrl)(new URL("htt/xx"))
 *  }}}
 *
 *  Create a `Catch` which logs exceptions using `handling` and `by`.
 *  {{{
 *  def log(t: Throwable): Unit = t.printStackTrace
 *
 *  val withThrowableLogging: Catch[Unit] = handling(classOf[MalformedURLException]) by (log)
 *
 *  def printUrl(url: String) : Unit = {
 *    val con = new URL(url) openConnection()
 *    val source = scala.io.Source.fromInputStream(con.getInputStream())
 *    source.getLines.foreach(println)
 *  }
 *
 *  val badUrl = "htt/xx"
 *  // Prints stacktrace,
 *  //   java.net.MalformedURLException: no protocol: htt/xx
 *  //     at java.net.URL.<init>(URL.java:586)
 *  withThrowableLogging { printUrl(badUrl) }
 *
 *  val goodUrl = "http://www.scala-lang.org/"
 *  // Prints page content,
 *  //   &lt;!DOCTYPE html&gt;
 *  //   &lt;html&gt;
 *  withThrowableLogging { printUrl(goodUrl) }
 *  }}}
 *
 *  Use `unwrapping` to create a `Catch` that unwraps exceptions before rethrowing.
 *  {{{
 *  class AppException(cause: Throwable) extends RuntimeException(cause)
 *
 *  val unwrappingCatch: Catch[Nothing] = unwrapping(classOf[AppException])
 *
 *  def calcResult: Int = throw new AppException(new NullPointerException)
 *
 *  // Throws NPE not AppException,
 *  //   java.lang.NullPointerException
 *  //     at .calcResult(&lt;console&gt;:17)
 *  val result = unwrappingCatch(calcResult)
 *  }}}
 *
 *  Use `failAsValue` to provide a default when a specified exception is caught.
 *
 *  {{{
 *  val inputDefaulting: Catch[Int] = failAsValue(classOf[NumberFormatException])(0)
 *  val candidatePick = "seven" // scala.io.StdIn.readLine()
 *
 *  // Int = 0
 *  val pick = inputDefaulting(candidatePick.toInt)
 *  }}}
 *
 *  Compose multiple `Catch`s with `or` to build a `Catch` that provides default values varied by exception.
 *  {{{
 *  val formatDefaulting: Catch[Int] = failAsValue(classOf[NumberFormatException])(0)
 *  val nullDefaulting: Catch[Int] = failAsValue(classOf[NullPointerException])(-1)
 *  val otherDefaulting: Catch[Int] = nonFatalCatch withApply(_ => -100)
 *
 *  val combinedDefaulting: Catch[Int] = formatDefaulting or nullDefaulting or otherDefaulting
 *
 *  def p(s: String): Int = s.length * s.toInt
 *
 *  // Int = 0
 *  combinedDefaulting(p("tenty-nine"))
 *
 *  // Int = -1
 *  combinedDefaulting(p(null: String))
 *
 *  // Int = -100
 *  combinedDefaulting(throw new IllegalStateException)
 *
 *  // Int = 22
 *  combinedDefaulting(p("11"))
 *  }}}
 *
 *  @groupname composition-catch Catch behavior composition
 *  @groupprio composition-catch 10
 *  @groupdesc composition-catch Build Catch objects from exception lists and catch logic
 *
 *  @groupname composition-finally Finally behavior composition
 *  @groupprio composition-finally 20
 *  @groupdesc composition-finally Build Catch objects from finally logic
 *
 *  @groupname canned-behavior General purpose catch objects
 *  @groupprio canned-behavior 30
 *  @groupdesc canned-behavior Catch objects with predefined behavior. Use combinator methods to compose additional behavior.
 *
 *  @groupname dsl DSL behavior composition
 *  @groupprio dsl 40
 *  @groupdesc dsl Expressive Catch behavior composition
 *
 *  @groupname composition-catch-promiscuously Promiscuous Catch behaviors
 *  @groupprio composition-catch-promiscuously 50
 *  @groupdesc composition-catch-promiscuously Useful if catching `ControlThrowable` or `InterruptedException` is required.
 *
 *  @groupname logic-container Logic Containers
 *  @groupprio logic-container 60
 *  @groupdesc logic-container Containers for catch and finally behavior.
 *
 *  @define protectedExceptions `ControlThrowable` or `InterruptedException`
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
   *  @return true if `x` is $protectedExceptions otherwise false.
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

  /** A container class for finally code.
   *  @group logic-container
   */
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
   *  @tparam T result type of bodies used in try and catch blocks
   *  @param pf Partial function used when applying catch logic to determine result value
   *  @param fin Finally logic which if defined will be invoked after catch logic
   *  @param rethrow Predicate on throwables determining when to rethrow a caught [[Throwable]]
   *  @group logic-container
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

    /** Create a new Catch container from this object and the supplied finally body.
     *  @param body The additional logic to apply after all existing finally bodies
     */
    def andFinally(body: => Unit): Catch[T] = {
      val appendedFin = fin map(_ and body) getOrElse new Finally(body)
      new Catch(pf, Some(appendedFin), rethrow)
    }

    /** Apply this catch logic to the supplied body, mapping the result
     *  into `Option[T]` - `None` if any exception was caught, `Some(T)` otherwise.
     */
    def opt[U >: T](body: => U): Option[U] = toOption(Some(body))

    /** Apply this catch logic to the supplied body, mapping the result
     *  into `Either[Throwable, T]` - `Left(exception)` if an exception was caught,
     *  `Right(T)` otherwise.
     */
    def either[U >: T](body: => U): Either[Throwable, U] = toEither(Right(body))

    /** Apply this catch logic to the supplied body, mapping the result
     * into `Try[T]` - `Failure` if an exception was caught, `Success(T)` otherwise.
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

  /** The empty `Catch` object.
   *  @group canned-behavior
   **/
  final val noCatch: Catch[Nothing] = new Catch(nothingCatcher) withDesc "<nothing>"

  /** A `Catch` object which catches everything.
   *  @group canned-behavior
   **/
  final def allCatch[T]: Catch[T] = new Catch(allCatcher[T]) withDesc "<everything>"

  /** A `Catch` object which catches non-fatal exceptions.
   *  @group canned-behavior
   **/
  final def nonFatalCatch[T]: Catch[T] = new Catch(nonFatalCatcher[T]) withDesc "<non-fatal>"

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Since the returned `Catch` object has no specific logic defined and will simply
   *  rethrow the exceptions it catches, you will typically want to call `opt`,
   *  `either` or `withTry` on the return value, or assign custom logic by calling "withApply".
   *
   *  Note that `Catch` objects automatically rethrow `ControlExceptions` and others
   *  which should only be caught in exceptional circumstances.  If you really want
   *  to catch exactly what you specify, use `catchingPromiscuously` instead.
   *  @group composition-catch
   */
  def catching[T](exceptions: Class[_]*): Catch[T] =
    new Catch(pfFromExceptions(exceptions : _*)) withDesc (exceptions map (_.getName) mkString ", ")

  def catching[T](c: Catcher[T]): Catch[T] = new Catch(c)

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Unlike "catching" which filters out those in shouldRethrow, this one will
   *  catch whatever you ask of it including $protectedExceptions.
   *  @group composition-catch-promiscuously
   */
  def catchingPromiscuously[T](exceptions: Class[_]*): Catch[T] = catchingPromiscuously(pfFromExceptions(exceptions : _*))
  def catchingPromiscuously[T](c: Catcher[T]): Catch[T]         = new Catch(c, None, _ => false)

  /** Creates a `Catch` object which catches and ignores any of the supplied exceptions.
   *  @group composition-catch
   */
  def ignoring(exceptions: Class[_]*): Catch[Unit] =
    catching(exceptions: _*) withApply (_ => ())

  /** Creates a `Catch` object which maps all the supplied exceptions to `None`.
   *  @group composition-catch
   */
  def failing[T](exceptions: Class[_]*): Catch[Option[T]] =
    catching(exceptions: _*) withApply (_ => None)

  /** Creates a `Catch` object which maps all the supplied exceptions to the given value.
   *  @group composition-catch
   */
  def failAsValue[T](exceptions: Class[_]*)(value: => T): Catch[T] =
    catching(exceptions: _*) withApply (_ => value)

  class By[T,R](f: T => R) {
    def by(x: T): R = f(x)
  }

  /** Returns a partially constructed `Catch` object, which you must give
    * an exception handler function as an argument to `by`.
    * @example
    * {{{
    *   handling(classOf[MalformedURLException], classOf[NullPointerException]) by (_.printStackTrace)
    * }}}
    *  @group dsl
    */
  // TODO: Add return type
  def handling[T](exceptions: Class[_]*) = {
    def fun(f: Throwable => T) = catching(exceptions: _*) withApply f
    new By[Throwable => T, Catch[T]](fun _)
  }

  /** Returns a `Catch` object with no catch logic and the argument as the finally logic.
   *  @group composition-finally
   */
  def ultimately[T](body: => Unit): Catch[T] = noCatch andFinally body

  /** Creates a `Catch` object which unwraps any of the supplied exceptions.
   *  @group composition-catch
   */
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
