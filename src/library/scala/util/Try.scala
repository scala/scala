/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util

/**
 * The Try type represents a computation that may either result in an exception, 
 * or return a success value. It's analagous to the `Either` type. 
 *
 */
sealed abstract class Try[+T] {
  /**
   * Returns true if the Try is a Failure, false otherwise.
   */
  def isFailure: Boolean

  /**
   * Returns true if the Try is a Success, false otherwise.
   */
  def isSuccess: Boolean

  /**
   * Returns the value from this Success or the given argument if this is a Failure.
   */
  def getOrElse[U >: T](default: => U) = if (isSuccess) apply() else default

  /**
   * Returns the value from this Success or throws the exception if this is a Failure
   */
  def apply(): T

  /**
   * Returns the value from this Success or throws the exception if this is a Failure.
   * Alias for apply()
   */
  def get = apply()

  /**
   * Applies the given function f if this is a Result.
   */
  def foreach[U](f: T => U) { onSuccess(f) }

  /**
   * Returns the given function applied to the value from this Success or returns this if this is a Failure.
   *
   */
  def flatMap[U](f: T => Try[U]): Try[U]

  /**
   * Maps the given function to the value from this Success or returns this if this is a Failure
   */
  def map[U](f: T => U): Try[U]

  /**
   * Converts this to a Failure if the predicate is not satisfied.
   */
  def filter(p: T => Boolean): Try[T]

  /**
   * Calls the exceptionHandler with the exception if this is a Failure. This is like flatMap for the exception.
   */
  def rescue[U >: T](rescueException: PartialFunction[Throwable, Try[U]]): Try[U]

  /**
   * Calls the exceptionHandler with the exception if this is a Failure. This is like map for the exception.
   */
  def handle[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U]

  /**
   * Invoked only if the computation was successful.  
   */
  def onSuccess[U](f: T => U): Try[T]

  /**
   * Invoked only if the computation failed.  
   */
  def onFailure[U](rescueException: Throwable => U): Try[T]

  /**
   * Invoked regardless of whether the computation completed
   * successfully or unsuccessfully.  Implemented in terms of
   * `respond` so that subclasses control evaluation order.  Returns a
   * chained `this` as in `respond`.
   */
  def ensure[U](f: => U): Try[T] =
    respond { _ => f }

  /**
   * Returns None if this is a Failure or a Some containing the value if this is a Success
   */
  def toOption = if (isSuccess) Some(apply()) else None

  /**
   * Invokes the given closure when the value is available.  Returns
   * another 'This[R]' that is guaranteed to be available only *after*
   * 'k' has run.  This enables the enforcement of invocation ordering.
   *
   * This is overridden by subclasses.
   */
  def respond[U](k: Try[T] => U): Try[T] = { 
    k(this)
    this
  }

  /**
   * Invokes the given transformation when the value is available,
   * returning the transformed value. This method is like a combination
   * of flatMap and rescue. This method is typically used for more
   * imperative control-flow than flatMap/rescue which often exploits
   * the Null Object Pattern.
   *
   * This is overridden by subclasses.
   */
  def transform[U](f: Try[T] => Try[U]): Try[U] =
    f(this)

  /**
   * Returns the given function applied to the value from this Success or returns this if this is a Failure.
   * Alias for flatMap
   */
  def andThen[U](f: T => Try[U]) = flatMap(f)

  /**
   * Transforms a nested Try, i.e., a Try of type `Try[Try[T]]`, 
   * into an un-nested Try, i.e., a Try of type `Try[T]`
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U]
}

final case class Failure[+T](e: Throwable) extends Try[T] {
  def isFailure = true
  def isSuccess = false
  def rescue[U >: T](rescueException: PartialFunction[Throwable, Try[U]]): Try[U] = {
    try {
      if (rescueException.isDefinedAt(e)) rescueException(e) else this
    } catch {
      case e2 => Failure(e2)
    }   
  }
  def apply(): T = throw e
  def flatMap[U](f: T => Try[U]): Try[U] = Failure[U](e)
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = Failure[U](e)
  def map[U](f: T => U): Try[U] = Failure[U](e)
  def filter(p: T => Boolean): Try[T] = this
  def onFailure[U](rescueException: Throwable => U): Try[T] = {
    rescueException(e)
    this
  }
  def onSuccess[U](f: T => U): Try[T] = this
  def handle[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U] = 
    if (rescueException.isDefinedAt(e)) {
      Try(rescueException(e))
    } else {
      this
    }
}

final case class Success[+T](r: T) extends Try[T] {
  def isFailure = false
  def isSuccess = true
  def rescue[U >: T](rescueException: PartialFunction[Throwable, Try[U]]): Try[U] = Success(r)
  def apply() = r
  def flatMap[U](f: T => Try[U]): Try[U] = 
    try f(r) 
    catch { 
      case e => Failure(e) 
    }
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = r
  def map[U](f: T => U): Try[U] = Try[U](f(r))
  def filter(p: T => Boolean): Try[T] = 
    if (p(apply())) this 
    else Failure(new NoSuchElementException("Predicate does not hold"))
  def onFailure[U](rescueException: Throwable => U): Try[T] = this  
  def onSuccess[U](f: T => U): Try[T] = {
    f(r)
    this
  }
  def handle[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U] = this
}

object Try {

  def apply[T](r: => T): Try[T] = {
    try { Success(r) } catch {
      case e => Failure(e)
    }
  }

}