/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util

import collection.Seq
import scala.util.control.NonFatal
import language.implicitConversions

/**
 * The `Try` type represents a computation that may either result in an exception, or return a
 * successfully computed value. It's similar to, but semantically different from the [[scala.util.Either]] type.
 *
 * Instances of `Try[T]`, are either an instance of [[scala.util.Success]][T] or [[scala.util.Failure]][T].
 *
 * For example, `Try` can be used to perform division on a user-defined input, without the need to do explicit
 * exception-handling in all of the places that an exception might occur.
 *
 * Example:
 * {{{
 *   import scala.util.{Try, Success, Failure}
 *
 *   def divide: Try[Int] = {
 *     val dividend = Try(Console.readLine("Enter an Int that you'd like to divide:\n").toInt)
 *     val divisor = Try(Console.readLine("Enter an Int that you'd like to divide by:\n").toInt)
 *     val problem = dividend.flatMap(x => divisor.map(y => x/y))
 *     problem match {
 *       case Success(v) =>
 *         println("Result of " + dividend.get + "/"+ divisor.get +" is: " + v)
 *         Success(v)
 *       case Failure(e) =>
 *         println("You must've divided by zero or entered something that's not an Int. Try again!")
 *         println("Info from the exception: " + e.getMessage)
 *         divide
 *     }
 *   }
 *
 * }}}
 *
 * An important property of `Try` shown in the above example is its ability to ''pipeline'', or chain, operations,
 * catching exceptions along the way. The `flatMap` and `map` combinators in the above example each essentially
 * pass off either their successfully completed value, wrapped in the `Success` type for it to be further operated
 * upon by the next combinator in the chain, or the exception wrapped in the `Failure` type usually to be simply
 * passed on down the chain. Combinators such as `rescue` and `recover` are designed to provide some type of
 * default behavior in the case of failure.
 *
 * ''Note'': only non-fatal exceptions are caught by the combinators on `Try` (see [[scala.util.control.NonFatal]]).
 * Serious system errors, on the other hand, will be thrown.
 *
 * `Try` comes to the Scala standard library after years of use as an integral part of Twitter's stack.
 *
 * @author based on Twitter's original implementation in com.twitter.util.
 * @since 2.10
 */
sealed abstract class Try[+T] {

  /** Returns `true` if the `Try` is a `Failure`, `false` otherwise.
   */
  def isFailure: Boolean

  /** Returns `true` if the `Try` is a `Success`, `false` otherwise.
   */
  def isSuccess: Boolean

  /** Returns the value from this `Success` or the given `default` argument if this is a `Failure`.
   */
  def getOrElse[U >: T](default: => U) = if (isSuccess) get else default

  /** Returns this `Try` if it's a `Success` or the given `default` argument if this is a `Failure`.
   */
  def orElse[U >: T](default: => Try[U]) = if (isSuccess) this else default

  /** Returns the value from this `Success` or throws the exception if this is a `Failure`.
   */
  def get: T

  /**
   * Applies the given function `f` if this is a `Success`, otherwise returns `Unit` if this is a `Failure`.
   */
  def foreach[U](f: T => U): Unit

  /**
   * Returns the given function applied to the value from this `Success` or returns this if this is a `Failure`.
   */
  def flatMap[U](f: T => Try[U]): Try[U]

  /**
   * Maps the given function to the value from this `Success` or returns this if this is a `Failure`.
   */
  def map[U](f: T => U): Try[U]

  /**
   * Converts this to a `Failure` if the predicate is not satisfied.
   */
  def filter(p: T => Boolean): Try[T]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like `flatMap` for the exception.
   */
  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like map for the exception.
   */
  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U]

  /**
   * Returns `None` if this is a `Failure` or a `Some` containing the value if this is a `Success`.
   */
  def toOption = if (isSuccess) Some(get) else None

  /**
   * Transforms a nested `Try`, ie, a `Try` of type `Try[Try[T]]`,
   * into an un-nested `Try`, ie, a `Try` of type `Try[T]`.
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U]

  /**
   * Completes this `Try` with an exception wrapped in a `Success`. The exception is either the exception that the
   * `Try` failed with (if a `Failure`) or an `UnsupportedOperationException`.
   */
  def failed: Try[Throwable]

  /** Completes this `Try` by applying the function `f` to this if this is of type `Failure`, or conversely, by applying
   *  `s` if this is a `Success`.
   */
  def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U] = this match {
    case Success(v) => s(v)
    case Failure(e) => f(e)
  }

}

object Try {

  def apply[T](r: => T): Try[T] = {
    try { Success(r) } catch {
      case NonFatal(e) => Failure(e)
    }
  }

}

final case class Failure[+T](val exception: Throwable) extends Try[T] {
  def isFailure: Boolean = true
  def isSuccess: Boolean = false
  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] =
    if (f.isDefinedAt(exception)) f(exception) else this
  def get: T = throw exception
  def flatMap[U](f: T => Try[U]): Try[U] = Failure[U](exception)
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = Failure[U](exception)
  def foreach[U](f: T => U): Unit = {}
  def map[U](f: T => U): Try[U] = Failure[U](exception)
  def filter(p: T => Boolean): Try[T] = this
  def recover[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U] = {
    try {
      if (rescueException.isDefinedAt(exception)) {
        Try(rescueException(exception))
      } else {
        this
      }
    } catch {
      case NonFatal(e) => Failure(e)
    }
  }
  def failed: Try[Throwable] = Success(exception)
}


final case class Success[+T](value: T) extends Try[T] {
  def isFailure: Boolean = false
  def isSuccess: Boolean = true
  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] = Success(value)
  def get = value
  def flatMap[U](f: T => Try[U]): Try[U] =
    try f(value)
    catch {
      case NonFatal(e) => Failure(e)
    }
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = value
  def foreach[U](f: T => U): Unit = f(value)
  def map[U](f: T => U): Try[U] = Try[U](f(value))
  def filter(p: T => Boolean): Try[T] = {
    try {
      if (p(value)) this
      else Failure(new NoSuchElementException("Predicate does not hold for " + value))
    } catch {
      case NonFatal(e) => Failure(e)
    }
  }
  def recover[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U] = this
  def failed: Try[Throwable] = Failure(new UnsupportedOperationException("Success.failed"))
}
