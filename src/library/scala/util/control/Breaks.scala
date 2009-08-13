/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.control

/** An object that can be used for the break control abstraction.
 *  Example usage:<pre>
 *
 *  <b>import</b> Breaks.{break, breakable}
 *
 *  breakable {
 *    <b>for</b> (...) {
 *      <b>if</b> (...) break
 *    }
 *  }</pre>
 *
 */
class Breaks {

  private val breakException = new BreakException

  /** A block from which one can exit with a `break''. */
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakException =>
        if (ex ne breakException) throw ex
    }
  }

  /* Break from closest enclosing breakable block */
  def break { throw breakException }
}

/** A singleton object providing the Break functionality */
object Breaks extends Breaks

private class BreakException extends RuntimeException with ControlException

