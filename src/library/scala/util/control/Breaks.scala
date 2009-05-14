package scala.util.control

/** An object that can be used for the break control abstraction.
 *  Example usage:
 *
 *  import Breaks.{break, breakable}
 *
 *  breakable {
 *    for (...) {
 *      if (...) break
 *    }
 *  }
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

private class BreakException extends RuntimeException

