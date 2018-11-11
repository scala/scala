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

package scala.util.control

/** A parent class for throwable objects intended for flow control.
 *
 *  Instances of `ControlThrowable` should not normally be caught.
 *
 *  As a convenience, `NonFatal` does not match `ControlThrowable`.
 *
 *  {{{
 *  import scala.util.control.{Breaks, NonFatal}, Breaks.{break, breakable}
 *
 *  breakable {
 *    for (v <- values) {
 *      try {
 *        if (p(v)) break
 *        else ???
 *      } catch {
 *        case NonFatal(t) => log(t)  // can't catch a break
 *      }
 *    }
 *  }
 *  }}}
 *
 *  Suppression is disabled, because flow control should not suppress
 *  an exceptional condition.
 *
 *  Instances of `ControlThrowable` should not normally have either
 *  a cause or a writable stack trace.
 *
 *  Legacy subclasses may make the stack trace writable by using
 *  the appropriate constructor. A cause may be set using `initCause`.
 *
 *  @author Miles Sabin
 */
abstract class ControlThrowable private[this] (message: String, cause: Throwable, writableStackTrace: Boolean) extends Throwable(message, cause, /*enableSuppression=*/ false, writableStackTrace) {
  def this() = this(message = null, cause = null, writableStackTrace = false)
  def this(message: String) = this(message = message, cause = null, writableStackTrace = false)
  @deprecated("Writable stack trace is provided only for compatibility", since = "2.13.0")
  protected def this(message: String, writableStackTrace: true) = this(message = message, cause = null, writableStackTrace = writableStackTrace)
}
