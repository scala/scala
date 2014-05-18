object Test {
  def loadField(x: scala.runtime.IntRef): Unit = x.elem
  def storeField(x: scala.runtime.IntRef): Unit = x.elem = 42

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }

  def main(args: Array[String]) {
    check(loadField(null)) // bug: did not NPE under -Ydead-code
    check(storeField(null))

  }
}
