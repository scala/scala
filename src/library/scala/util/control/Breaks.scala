/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

/** A class that can be instantiated for the break control abstraction.
 *  Example usage:
 *  {{{
 *  val mybreaks = new Breaks
 *  import mybreaks.{break, breakable}
 *
 *  breakable {
 *    for (...) {
 *      if (...) break()
 *    }
 *  }
 *  }}}
 *  Calls to break from one instantiation of `Breaks` will never
 *  target breakable objects of some other instantiation.
 */
class Breaks {

  private val breakException = new BreakControl

  /**
   * A block from which one can exit with a `break`. The `break` may be
   * executed further down in the call stack provided that it is called on the
   * exact same instance of `Breaks`.
   */
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakControl =>
        if (ex ne breakException) throw ex
    }
  }

  sealed trait TryBlock[T] {
    def catchBreak(onBreak: =>T): T
  }

  /**
   * This variant enables the execution of a code block in case of a `break()`:
   * {{{
   * tryBreakable {
   *   for (...) {
   *     if (...) break()
   *   }
   * } catchBreak {
   *   doCleanup()
   * }
   * }}}
   */
  def tryBreakable[T](op: =>T) = new TryBlock[T] {
    def catchBreak(onBreak: =>T) = try {
      op
    } catch {
      case ex: BreakControl =>
        if (ex ne breakException) throw ex
        onBreak
    }
  }

  /**
   * Break from dynamically closest enclosing breakable block using this exact
   * `Breaks` instance.
   *
   * @note This might be different than the statically closest enclosing block!
   */
  def break(): Nothing = { throw breakException }
}

/** An object that can be used for the break control abstraction.
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
