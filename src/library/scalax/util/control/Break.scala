package scala.util.control

object Break {
  private class BreakException extends RuntimeException
  private val breakException = new BreakException
  def break { throw breakException }
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakException =>
    }
  }
}

