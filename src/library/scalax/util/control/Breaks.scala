package scalax.util.control

object Breaks {
  private class BreakException extends RuntimeException
  private val breakException = new BreakException
  private class ContinueException extends RuntimeException
  private val continueException = new BreakException
  def break { throw breakException }
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakException =>
    }
  }
  def continue { throw continueException }
  def continuable(op: => Unit) {
    try {
      op
    } catch {
      case ex: ContinueException =>
	continuable(op)
    }
  }
}
