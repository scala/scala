
trait G {
  val c: Char = '\u000a'   // Allowed!
  def x = 9
  def y() = x
  def z() = {
    y()
    ()           // was Int does not take parameters
  }
  def v = y()
  ()     // was Int does not take parameters
  def w = { x() }       // There is a FF (^L) between the `x` and the `()`, which may be hardly or invisible
}
