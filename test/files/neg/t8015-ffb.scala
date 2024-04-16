//> using options -Xlint -Xfatal-warnings
//

trait G {
  val c: Char = '\u000a'   // allowed!
  def x = 9
  def y() = x
  def z() = {
    y()()           // was Int does not take parameters
  }
  def v = y()()     // was Int does not take parameters
  def w = { x() }       // ^L is colored blue on this screen, hardly visible
}
