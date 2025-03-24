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
 *  an exceptional condition. Stack traces are also disabled, allowing
 *  instances of `ControlThrowable` to be safely reused.
 *
 *  Instances of `ControlThrowable` should not normally have a cause.
 *  Legacy subclasses may set a cause using `initCause`.
 */
abstract class ControlThrowable(message: String) extends Throwable(
  message, /*cause*/ null, /*enableSuppression=*/ false, /*writableStackTrace*/ false) {

  def this() = this(message = null)
}
