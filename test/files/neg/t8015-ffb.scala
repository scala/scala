
trait G {
  val c: Char = '\u000a'   // disallowed!
  def x\u000d\u000a = 9    // as nl
  def y() = x
  def z() = {
    y()\u000a()           // was Int does not take parameters
  }
  def v = y()\u000c()     // was Int does not take parameters
  def w = { x() }       // ^L is colored blue on this screen, hardly visible
}
