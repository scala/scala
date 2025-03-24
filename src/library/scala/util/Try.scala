/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util

import scala.runtime.Statics
import scala.util.control.NonFatal

/**
 * The `Try` type represents a computation that may fail during evaluation by raising an exception.
 * It holds either a successfully computed value or the exception that was thrown.
 * This is similar to the [[scala.util.Either]] type, but with different semantics.
 *
 * Instances of `Try[T]` are an instance of either [[scala.util.Success]][T] or [[scala.util.Failure]][T].
 *
 * For example, consider a computation that performs division on user-defined input.
 * `Try` can reduce or eliminate the need for explicit exception handling in all of the places
 * where an exception might be thrown.
 *
 * Example:
 * {{{
 *   import scala.io.StdIn
 *   import scala.util.{Try, Success, Failure}
 *
 *   def divide: Try[Int] = {
 *     val dividend = Try(StdIn.readLine("Enter an Int that you'd like to divide:\n").toInt)
 *     val divisor = Try(StdIn.readLine("Enter an Int that you'd like to divide by:\n").toInt)
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
 * passed on down the chain. Combinators such as `recover` and `recoverWith` are designed to provide some type of
 * default behavior in the case of failure.
 *
 * ''Note'': only non-fatal exceptions are caught by the combinators on `Try` (see [[scala.util.control.NonFatal]]).
 * Serious system errors, on the other hand, will be thrown.
 *
 * ''Note:'': all Try combinators will catch exceptions and return failure unless otherwise specified in the documentation.
 */
sealed abstract class Try[+T] extends Product with Serializable {

  /** Returns `true` if the `Try` is a `Failure`, `false` otherwise.
   */
  def isFailure: Boolean

  /** Returns `true` if the `Try` is a `Success`, `false` otherwise.
   */
  def isSuccess: Boolean

  /** Returns the value from this `Success` or the given `default` argument if this is a `Failure`.
   *
   * ''Note:'': This will throw an exception if it is not a success and default throws an exception.
   */
  def getOrElse[U >: T](default: => U): U

  /** Returns this `Try` if it's a `Success` or the given `default` argument if this is a `Failure`.
   */
  def orElse[U >: T](default: => Try[U]): Try[U]

  /** Returns the value from this `Success` or throws the exception if this is a `Failure`.
   */
  def get: T

  /**
   * Applies the given function `f` if this is a `Success`, otherwise returns `Unit` if this is a `Failure`.
   *
   * ''Note:'' If `f` throws, then this method may throw an exception.
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
   * Applies the given partial function to the value from this `Success` or returns this if this is a `Failure`.
   */
  def collect[U](pf: PartialFunction[T, U]): Try[U]

  /**
   * Converts this to a `Failure` if the predicate is not satisfied.
   */
  def filter(p: T => Boolean): Try[T]

  /** Creates a non-strict filter, which eventually converts this to a `Failure`
   *  if the predicate is not satisfied.
   *
   *  Note: unlike filter, withFilter does not create a new Try.
   *        Instead, it restricts the domain of subsequent
   *        `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *
   * As Try is a one-element collection, this may be a bit overkill,
   * but it's consistent with withFilter on Option and the other collections.
   *
   *  @param p   the predicate used to test elements.
   *  @return    an object of class `WithFilter`, which supports
   *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *             All these operations apply to those elements of this Try
   *             which satisfy the predicate `p`.
   */
  @inline final def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  final class WithFilter(p: T => Boolean) {
    def map[U](f:     T => U): Try[U]           = Try.this filter p map f
    def flatMap[U](f: T => Try[U]): Try[U]      = Try.this filter p flatMap f
    def foreach[U](f: T => U): Unit             = Try.this filter p foreach f
    def withFilter(q: T => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like `flatMap` for the exception.
   */
  def recoverWith[U >: T](pf: PartialFunction[Throwable, Try[U]]): Try[U]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like map for the exception.
   */
  def recover[U >: T](pf: PartialFunction[Throwable, U]): Try[U]

  /**
   * Returns `None` if this is a `Failure` or a `Some` containing the value if this is a `Success`.
   */
  def toOption: Option[T]

  /**
   * Transforms a nested `Try`, ie, a `Try` of type `Try[Try[T]]`,
   * into an un-nested `Try`, ie, a `Try` of type `Try[T]`.
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U]

  /**
   * Inverts this `Try`. If this is a `Failure`, returns its exception wrapped in a `Success`.
   * If this is a `Success`, returns a `Failure` containing an `UnsupportedOperationException`.
   */
  def failed: Try[Throwable]

  /** Completes this `Try` by applying the function `f` to this if this is of type `Failure`, or conversely, by applying
   *  `s` if this is a `Success`.
   */
  def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U]

  /**
   * Returns `Left` with `Throwable` if this is a `Failure`, otherwise returns `Right` with `Success` value.
   */
  def toEither: Either[Throwable, T]

  /**
   * Applies `fa` if this is a `Failure` or `fb` if this is a `Success`.
   * If `fb` is initially applied and throws an exception,
   * then `fa` is applied with this exception.
   *
   * @example {{{
   * val result: Try[Int] = Try { string.toInt }
   * log(result.fold(
   *   ex => "Operation failed with " + ex,
   *   v => "Operation produced value: " + v
   * ))
   * }}}
   *
   * @param fa the function to apply if this is a `Failure`
   * @param fb the function to apply if this is a `Success`
   * @return the results of applying the function
   */
  def fold[U](fa: Throwable => U, fb: T => U): U

}

object Try {
  /** Constructs a `Try` using the by-name parameter as a result value.
   *
   *  The evaluation of `r` is attempted once.
   *
   *  Any non-fatal exception is caught and results in a `Failure`
   *  that holds the exception.
   *
   *  @param r the result value to compute
   *  @return the result of evaluating the value, as a `Success` or `Failure`
   */
  def apply[T](r: => T): Try[T] =
    try {
      val r1 = r
      Success(r1)
    } catch {
      case NonFatal(e) => Failure(e)
    }
}

final case class Failure[+T](exception: Throwable) extends Try[T] {
  override def isFailure: Boolean = true
  override def isSuccess: Boolean = false
  override def get: T = throw exception
  override def getOrElse[U >: T](default: => U): U = default
  override def orElse[U >: T](default: => Try[U]): Try[U] =
    try default catch { case NonFatal(e) => Failure(e) }
  override def flatMap[U](f: T => Try[U]): Try[U] = this.asInstanceOf[Try[U]]
  override def flatten[U](implicit ev: T <:< Try[U]): Try[U] = this.asInstanceOf[Try[U]]
  override def foreach[U](f: T => U): Unit = ()
  override def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U] =
    try f(exception) catch { case NonFatal(e) => Failure(e) }
  override def map[U](f: T => U): Try[U] = this.asInstanceOf[Try[U]]
  override def collect[U](pf: PartialFunction[T, U]): Try[U] = this.asInstanceOf[Try[U]]
  override def filter(p: T => Boolean): Try[T] = this
  override def recover[U >: T](pf: PartialFunction[Throwable, U]): Try[U] = {
    val marker = Statics.pfMarker
    try {
      val v = pf.applyOrElse(exception, (x: Throwable) => marker)
      if (marker ne v.asInstanceOf[AnyRef]) Success(v.asInstanceOf[U]) else this
    } catch { case NonFatal(e) => Failure(e) }
  }
  override def recoverWith[U >: T](pf: PartialFunction[Throwable, Try[U]]): Try[U] = {
    val marker = Statics.pfMarker
    try {
      val v = pf.applyOrElse(exception, (x: Throwable) => marker)
      if (marker ne v.asInstanceOf[AnyRef]) v.asInstanceOf[Try[U]] else this
    } catch { case NonFatal(e) => Failure(e) }
  }
  override def failed: Try[Throwable] = Success(exception)
  override def toOption: Option[T] = None
  override def toEither: Either[Throwable, T] = Left(exception)
  override def fold[U](fa: Throwable => U, fb: T => U): U = fa(exception)
}

final case class Success[+T](value: T) extends Try[T] {
  override def isFailure: Boolean = false
  override def isSuccess: Boolean = true
  override def get = value
  override def getOrElse[U >: T](default: => U): U = get
  override def orElse[U >: T](default: => Try[U]): Try[U] = this
  override def flatMap[U](f: T => Try[U]): Try[U] =
    try f(value) catch { case NonFatal(e) => Failure(e) }
  override def flatten[U](implicit ev: T <:< Try[U]): Try[U] = value
  override def foreach[U](f: T => U): Unit = f(value)
  override def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U] = this flatMap s
  override def map[U](f: T => U): Try[U] = Try[U](f(value))
  override def collect[U](pf: PartialFunction[T, U]): Try[U] = {
    val marker = Statics.pfMarker
    try {
      val v = pf.applyOrElse(value, ((x: T) => marker).asInstanceOf[Function[T, U]])
      if (marker ne v.asInstanceOf[AnyRef]) Success(v)
      else Failure(new NoSuchElementException("Predicate does not hold for " + value))
    } catch { case NonFatal(e) => Failure(e) }
  }
  override def filter(p: T => Boolean): Try[T] =
    try {
      if (p(value)) this else Failure(new NoSuchElementException("Predicate does not hold for " + value))
    } catch { case NonFatal(e) => Failure(e) }
  override def recover[U >: T](pf: PartialFunction[Throwable, U]): Try[U] = this
  override def recoverWith[U >: T](pf: PartialFunction[Throwable, Try[U]]): Try[U] = this
  override def failed: Try[Throwable] = Failure(new UnsupportedOperationException("Success.failed"))
  override def toOption: Option[T] = Some(value)
  override def toEither: Either[Throwable, T] = Right(value)
  override def fold[U](fa: Throwable => U, fb: T => U): U =
    try { fb(value) } catch { case NonFatal(e) => fa(e) }
}
