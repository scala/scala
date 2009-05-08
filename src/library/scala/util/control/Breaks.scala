package scala.util.control

object Breaks {
  private class BreakException extends RuntimeException
  private val breakException = new BreakException
  private class ContinueException extends RuntimeException
  private val continueException = new ContinueException

  /** A block from which one can exit with a `break' and which can be resumed with a `continue'. */
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakException =>
      case ex: ContinueException => breakable(op)
    }
  }

  /* Break from closest enclosing breakable block */
  def break { throw breakException }

  /* Continue with start of closest enclosing breakable block */
  def continue { throw continueException }
}
