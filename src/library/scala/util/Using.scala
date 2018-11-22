/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.util

import java.util.concurrent.atomic.AtomicBoolean

import scala.util.control.{ControlThrowable, NonFatal}

/** A utility for performing automatic resource management. It can be used to perform an
  * operation using resources, after which it will release the resources, in reverse order
  * of their creation. The resource opening, operation, and resource releasing are wrapped
  * in a `Try`.
  *
  * If more than one exception is thrown by the operation and releasing resources,
  * the exception thrown ''first'' is returned within the `Try`, with the other exceptions
  * [[java.lang.Throwable.addSuppressed(Throwable) added as suppressed exceptions]]
  * to the one thrown first. This is the case ''unless'' a later exception is
  * [[scala.util.control.NonFatal fatal]], and the one preceding it is not. In that case,
  * the first exception is added as a suppressed exception to the fatal one, and the fatal
  * one is thrown. If an exception is a
  * [[scala.util.control.ControlThrowable ControlThrowable]], no exception will be added to
  * it as a suppressed exception.
  *
  * @example
  * {{{
  * val lines: Try[List[String]] = Using(resource1) { r1 =>
  *   r1.lines.toList
  * }
  * }}}
  *
  * @example
  * {{{
  * val lines: Try[Seq[String]] = for {
  *   r1 <- Using(resource1)
  *   r2 <- Using(resource2)
  *   r3 <- Using(resource3)
  *   r4 <- Using(resource4)
  * } yield {
  *   // use your resources here
  *   r1.lines ++ r2.lines ++ r3.lines ++ r4.lines
  * }
  * }}}
  */
final class Using[R] private(resource: => R) {
  private[this] val used = new AtomicBoolean(false)

  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception.
    *
    * @param f the operation to perform
    * @param r an implicit [[Using.Resource]]
    * @tparam A the return type of the operation
    * @throws java.lang.IllegalStateException if the resource has already been used
    * @return a [[scala.util.Try `Try`]] containing the result of the operation, or
    *         an exception if one was thrown by the operation or by releasing the resource
    */
  @throws[IllegalStateException]("if the resource has already been used")
  @inline def apply[A](f: R => A)(implicit r: Using.Resource[R]): Try[A] = map(f)

  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception.
    *
    * @param f the operation to perform
    * @param r an implicit [[Using.Resource]]
    * @tparam A the return type of the operation
    * @throws java.lang.IllegalStateException if the resource has already been used
    * @return a [[scala.util.Try `Try`]] containing the result of the operation, or
    *         an exception if one was thrown by the operation or by releasing the resource
    */
  @throws[IllegalStateException]("if the resource has already been used")
  def map[A](f: R => A)(implicit r: Using.Resource[R]): Try[A] = Try { useWith(f) }

  /** Performs an operation which returns a [[scala.util.Try `Try`]] using a resource,
    * and then releases the resource, even if the operation throws an exception.
    *
    * @param f the `Try`-returning operation to perform
    * @param r an implicit [[Using.Resource]]
    * @tparam A the return type of the operation
    * @throws java.lang.IllegalStateException if the resource has already been used
    * @return the result of the inner operation, or a [[scala.util.Try `Try`]]
    *         containing an exception if one was thrown by the operation or by
    *         releasing the resource
    */
  @throws[IllegalStateException]("if the resource has already been used")
  def flatMap[A](f: R => Try[A])(implicit r: Using.Resource[R]): Try[A] =
    map {
      r => f(r).get // otherwise inner Failure will be lost on exceptional release
    }

  @inline private[this] def useWith[A](f: R => A)(implicit r: Using.Resource[R]): A =
    if (used.getAndSet(true)) throw new IllegalStateException("resource has already been used")
    else Using.resource(resource)(f)
}

/** @define recommendUsing      It is highly recommended to use the `Using` construct,
  *                             which safely wraps resource usage and management in a `Try`.
  * @define suppressionBehavior If two exceptions are thrown (e.g. by an operation and closing a resource),
  *                             one of them rethrown, and the other is
  *                             [[java.lang.Throwable.addSuppressed(Throwable) added to it as a suppressed exception]].
  *                             If the two exceptions are of different 'severities' (see below), the one of a higher
  *                             severity is rethrown, and the one of a lower severity is added to it as a suppressed
  *                             exception. If the two exceptions are of the same severity, the one thrown first is
  *                             rethrown, and the one thrown second is added to it as a suppressed exception.
  *                             If an exception is a [[scala.util.control.ControlThrowable `ControlThrowable`]], or
  *                             if it does not support suppression (see
  *                             [[java.lang.Throwable `Throwable`'s constructor with an `enableSuppression` parameter]]),
  *                             an exception that would have been suppressed is instead discarded.
  *
  *                             Exceptions are ranked from highest to lowest severity as follows:
  *                             - `java.lang.VirtualMachineError`
  *                             - `java.lang.LinkageError`
  *                             - `java.lang.InterruptedException` and `java.lang.ThreadDeath`
  *                             - [[scala.util.control.NonFatal fatal exceptions]], excluding `scala.util.control.ControlThrowable`
  *                             - `scala.util.control.ControlThrowable`
  *                             - all other exceptions
  *
  *                             When more than two exceptions are thrown, the first two are combined and
  *                             rethrown as described above, and each successive exception thrown is combined
  *                             as it is thrown.
  */
object Using {
  /** Creates a `Using` from the given resource.
    *
    * @note If the resource does not have an implicit [[Resource]] in
    *       scope, the returned `Using` will be useless.
    */
  def apply[R](resource: => R): Using[R] = new Using(resource)

  private def preferentiallySuppress(primary: Throwable, secondary: Throwable): Throwable = {
    def score(t: Throwable): Int = t match {
      case _: VirtualMachineError                   => 4
      case _: LinkageError                          => 3
      case _: InterruptedException | _: ThreadDeath => 2
      case _: ControlThrowable                      => 0
      case e if !NonFatal(e)                        => 1 // in case this method gets out of sync with NonFatal
      case _                                        => -1
    }
    @inline def suppress(t: Throwable, suppressed: Throwable): Throwable = { t.addSuppressed(suppressed); t }

    if (score(secondary) > score(primary)) suppress(secondary, primary)
    else suppress(primary, secondary)
  }

  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception. This method behaves similarly
    * to Java's try-with-resources.
    *
    * $recommendUsing
    *
    * $suppressionBehavior
    *
    * @param resource the resource
    * @param body     the operation to perform with the resource
    * @tparam R the type of the resource
    * @tparam A the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resource throws
    */
  def resource[R: Resource, A](resource: R)(body: R => A): A = {
    if (resource == null) throw new NullPointerException("null resource")

    var primary: Throwable = null
    try {
      body(resource)
    } catch {
      case t: Throwable =>
        primary = t
        null.asInstanceOf[A] // compiler doesn't know `finally` will throw
    } finally {
      if (primary eq null) implicitly[Resource[R]].release(resource)
      else {
        var toThrow = primary
        try {
          implicitly[Resource[R]].release(resource)
        } catch {
          case other: Throwable => toThrow = preferentiallySuppress(primary, other)
        } finally {
          throw toThrow
        }
      }
    }
  }

  /** Performs an operation using two resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $recommendUsing
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Resource, R2: Resource, A](
      resource1: R1,
      resource2: => R2
    )(body: (R1, R2) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        body(r1, r2)
      }
    }

  /** Performs an operation using three resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $recommendUsing
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param resource3 the third resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam R3 the type of the third resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Resource, R2: Resource, R3: Resource, A](
      resource1: R1,
      resource2: => R2,
      resource3: => R3
    )(body: (R1, R2, R3) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        resource(resource3) { r3 =>
          body(r1, r2, r3)
        }
      }
    }

  /** Performs an operation using four resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $recommendUsing
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param resource3 the third resource
    * @param resource4 the fourth resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam R3 the type of the third resource
    * @tparam R4 the type of the fourth resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Resource, R2: Resource, R3: Resource, R4: Resource, A](
      resource1: R1,
      resource2: => R2,
      resource3: => R3,
      resource4: => R4
    )(body: (R1, R2, R3, R4) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        resource(resource3) { r3 =>
          resource(resource4) { r4 =>
            body(r1, r2, r3, r4)
          }
        }
      }
    }

  /** A typeclass describing a resource which can be released.
    *
    * @tparam R the type of the resource
    */
  trait Resource[-R] {
    /** Releases the specified resource. */
    def release(resource: R): Unit
  }

  object Resource {
    /** An implicit `Resource` for [[java.lang.AutoCloseable `AutoCloseable`s]]. */
    implicit val autoCloseableResource: Resource[AutoCloseable] = (resource: AutoCloseable) => resource.close()
  }

}
