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

/** Provides the `break` control abstraction.
 *
 *  The `break` method uses a `ControlThrowable` to transfer
 *  control up the stack to an enclosing `breakable`.
 *
 *  It is typically used to abruptly terminate a `for` loop,
 *  but can be used to return from an arbitrary computation.
 *
 *  Control resumes after the `breakable`.
 *
 *  If there is no matching `breakable`, the `BreakControl`
 *  thrown by `break` is handled in the usual way: if not
 *  caught, it may terminate the current `Thread`.
 *
 *  `BreakControl` carries no stack trace, so the default
 *  exception handler does not print useful diagnostic
 *  information; there is no compile-time warning if there
 *  is no matching `breakable`.
 *
 *  A catch clause using `NonFatal` is safe to use with
 *  `break`; it will not short-circuit the transfer
 *  of control to the enclosing `breakable`.
 *
 *  A `breakable` matches a call to `break` if the methods
 *  were invoked on the same receiver object, which may be the
 *  convenience value `Breaks`.
 *
 *  Example usage:
 *  {{{
 *  val mybreaks = new Breaks
 *  import mybreaks.{break, breakable}
 *
 *  breakable {
 *    for (x <- xs) {
 *      if (done) break()
 *      f(x)
 *    }
 *  }
 *  }}}
 *  Calls to `break` from one instance of `Breaks` will never
 *  resume at the `breakable` of some other instance.
 *
 *  Any intervening exception handlers should use `NonFatal`,
 *  or use `Try` for evaluation:
 *  {{{
 *  val mybreaks = new Breaks
 *  import mybreaks.{break, breakable}
 *
 *  breakable {
 *    for (x <- xs) Try { if (quit) break else f(x) }.foreach(println)
 *  }
 *  }}}
 */
class Breaks {

  private[this] val breakException = new BreakControl

  /** A block from which one can exit with a `break`. The `break` may be
   *  executed further down in the call stack provided that it is called on the
   *  exact same instance of `Breaks`.
   */
  def breakable(op: => Unit): Unit =
    try op catch { case ex: BreakControl if ex eq breakException => }

  sealed trait TryBlock[T] {
    def catchBreak(onBreak: => T): T
  }

  /** Try a computation that produces a value, supplying a default
   *  to be used if the computation terminates with a `break`.
   *
   * {{{
   * tryBreakable {
   *   (1 to 3).map(i => if (math.random < .5) break else i * 2)
   * } catchBreak {
   *   Vector.empty
   * }
   * }}}
   */
  def tryBreakable[T](op: => T): TryBlock[T] =
    new TryBlock[T] {
      def catchBreak(onBreak: => T) =
        try op catch { case ex: BreakControl if ex eq breakException => onBreak }
    }

  /** Break from the dynamically closest enclosing breakable block that also uses
   *  this `Breaks` instance.
   *
   *  @note This might be different from the statically closest enclosing block!
   *  @note Invocation without parentheses relies on the conversion to "empty application".
   */
  def break(): Nothing = throw breakException
}

/** An object that can be used for the break control abstraction.
 *
 *  Example usage:
 *  {{{
 *  import Breaks.{break, breakable}
 *
 *  breakable {
 *    for (...) {
 *      if (...) break
 *    }
 *  }
 *  }}}
 */
object Breaks extends Breaks

private class BreakControl extends ControlThrowable
