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

package scala
package util.control

/** A marker trait indicating that the `Throwable` it is mixed into is
 *  intended for flow control.
 *
 *  Note that `Throwable` subclasses which extend this trait may extend any
 *  other `Throwable` subclass (eg. `RuntimeException`) and are not required
 *  to extend `Throwable` directly.
 *
 *  Instances of `Throwable` subclasses marked in this way should not normally
 *  be caught. Where catch-all behaviour is required `ControlThrowable`
 *  should be propagated, for example:
 *  {{{
 *  import scala.util.control.ControlThrowable
 *
 *  try {
 *    // Body might throw arbitrarily
 *  } catch {
 *    case c: ControlThrowable => throw c // propagate
 *    case t: Exception        => log(t)  // log and suppress
 *  }
 *  }}}
 *
 *  @author Miles Sabin
 */
trait ControlThrowable extends Throwable with NoStackTrace
