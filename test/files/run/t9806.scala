object Ex extends Exception
object Test {
  def main(args: Array[String]) {
    try foo catch { case Ex => }
  }

  def isTrue(b: Boolean) = b
  def foo = {
    var streamErrors1 = true
    try {
      streamErrors1 = false
      throw Ex
    } catch {
      case ex if streamErrors1 =>
        assert(isTrue(streamErrors1))
    }
  }
}
