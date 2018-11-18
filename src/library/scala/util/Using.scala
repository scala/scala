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
  * The `ResourceUse` class encapsulates the creation of a resource, and an operation
  * (from `map` or `flatMap`) to perform using the resource.
  *
  * @example
  * {{{
  * val lines: Try[List[String]] = Using(resource1) { r1 =>
  *   // use your resource here
  *   r1.lines.toList
  * }
  * }}}
  *
  * @example
  * {{{
  * val lines: Try[Seq[String]] = {
  *   for {
  *     r1 <- Using(resource1)
  *     r2 <- Using(resource2)
  *     r3 <- Using(resource3)
  *     r4 <- Using(resource4)
  *   } yield {
  *     // use your resources here
  *     r1.lines ++ r2.lines ++ r3.lines ++ r4.lines
  *   }
  * }.use()
  * }}}
  *
  * @define recommendUsing                   It is highly recommended to use the `ResourceUse` construct,
  *                                          which safely wraps resource usage and management in a `Try`.
  * @define multiResourceSuppressionBehavior If more than one exception is thrown by the operation and releasing resources,
  *                                          the exception thrown ''first'' is thrown, with the other exceptions
  *                                          [[java.lang.Throwable.addSuppressed(Throwable) added as suppressed exceptions]]
  *                                          to the one thrown first. This is the case ''unless'' a later exception is
  *                                          [[scala.util.control.NonFatal fatal]], and the one preceding it is not. In that case,
  *                                          the first exception is added as a suppressed exception to the fatal one, and the fatal
  *                                          one is thrown. If an exception is a
  *                                          [[scala.util.control.ControlThrowable ControlThrowable]], no exception will be added to
  *                                          it as a suppressed exception.
  */
object Using {
  private final class SingleUse[R](resource: => R) {
    private[this] val used = new AtomicBoolean(false)

    def useWith[A](f: R => A)(implicit r: Using.Resource[R]): A =
      if (used.getAndSet(true)) throw new IllegalStateException("resource has already been used")
      else Using.resource(resource)(f)
  }

  private val cachedIdentity: Any => Any = identity

  final class ResourceUse[R, A] private[Using](resource: SingleUse[R], op: R => A) {
    /** Performs an operation on the result of this `ResourceUse`'s existing operation
      * using its resource, and then releases the resource,
      * even if the operation throws an exception.
      *
      * @param f the operation to perform
      * @param r an implicit [[Using.Resource]]
      * @tparam B the return type of the operation
      * @throws java.lang.IllegalStateException if the resource has already been used
      * @return a [[scala.util.Try `Try`]] containing the result of the operation, or
      *         an exception if one was thrown by the operation or by releasing the resource
      */
    @throws[IllegalStateException]("if the resource has already been used")
    def apply[B](f: A => B)(implicit r: Resource[R]): Try[B] = map(f).use()

    /** Returns a `ResourceUse` which will perform the specified operation (after this `ResourceUse`'s
      * operation) when [[use() used]].
      *
      * @param f the operation to perform
      * @tparam B the return type of the operation
      */
    def map[B](f: A => B): ResourceUse[R, B] =
      new ResourceUse(resource, if (op eq cachedIdentity) f.asInstanceOf[R => B] else r => f(op(r)))

    /** Returns a `ResourceUse` which will perform the specified `ResourceUse`-returning operation
      * and [[use() use]] the returned `ResourceUse` (after this `ResourceUse`'s operation) when [[use() used]].
      *
      * @param f the `ResourceUse`-returning operation to perform
      * @tparam B the return type of the returned `ResourceUse`'s operation(s)
      */
    def flatMap[R1: Resource, B](f: A => ResourceUse[R1, B]): ResourceUse[R, B] =
      new ResourceUse(resource, r => f(op(r)).use0())

    /** Performs an operation on the result of this `ResourceUse`'s existing operation
      * using its resource, and then releases the resource,
      * even if the operation throws an exception.
      *
      * @param f the operation to perform
      * @param r an implicit [[Using.Resource]]
      * @throws java.lang.IllegalStateException if the resource has already been used
      */
    def foreach[U](f: A => U)(implicit r: Resource[R]): Unit = {
      map(f).use()
      ()
    }

    /** Performs this `ResourceUse`'s operation(s) using its resource, and then releases the resource,
      * even if the operation throws an exception.
      *
      * @param r an implicit [[Using.Resource]]
      * @throws java.lang.IllegalStateException if the resource has already been used
      */
    def use()(implicit r: Resource[R]): Try[A] = Try { use0() }

    private def use0()(implicit r: Resource[R]): A = resource.useWith(op)
  }

  /** Creates a `ResourceUse` from the given resource.
    *
    * @note In order to allow certain usages of `ResourceUse`, the implicit
    *       [[Resource `Resource`]] is a parameter on the [[ResourceUse.map `map`]],
    *       [[ResourceUse.flatMap, `flatMap`]] and [[ResourceUse.apply `apply`]]
    *       methods, rather than on the construction of the `ResourceUse`.
    *       Consequently, it is possible to create a `ResourceUse` for which
    *       there is no `Resource`; in that case, the `ResourceUse` returned
    *       by this method will not be usable.
    */
  def apply[R](resource: => R): ResourceUse[R, R] =
    new ResourceUse(new SingleUse(resource), cachedIdentity.asInstanceOf[R => R])

  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception. This method behaves similarly
    * to Java's try-with-resources.
    *
    * $recommendUsing
    *
    * If both the operation and releasing the resource throw exceptions, the one thrown
    * when releasing the resource is
    * [[java.lang.Throwable.addSuppressed(Throwable) added as a suppressed exception]]
    * to the one thrown by the operation, ''unless'' the exception thrown when releasing
    * the resource is [[scala.util.control.NonFatal fatal]], and the one thrown by the
    * operation is not. In that case, the exception thrown by the operation is added
    * as a suppressed exception to the one thrown when releasing the resource. If an
    * exception is a [[scala.util.control.ControlThrowable ControlThrowable]], no
    * exception will be added to it as a suppressed exception.
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

    @inline def safeAddSuppressed(t: Throwable, suppressed: Throwable): Unit = {
      // don't `addSuppressed` to something which is a `ControlThrowable`
      if (!t.isInstanceOf[ControlThrowable]) t.addSuppressed(suppressed)
    }

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
          case other: Throwable =>
            if (NonFatal(primary) && !NonFatal(other)) {
              // `other` is fatal, `primary` is not
              toThrow = other
              safeAddSuppressed(other, primary)
            } else {
              // `toThrow` is already `primary`
              safeAddSuppressed(primary, other)
            }
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
    * $multiResourceSuppressionBehavior
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
    * $multiResourceSuppressionBehavior
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
    * $multiResourceSuppressionBehavior
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
