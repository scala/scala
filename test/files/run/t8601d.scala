object Test {
  def monitor(x: AnyRef): Unit = {x.synchronized(()); ()}
  def check(x: => Any) = try { x; sys.error("failed to throw NPE") } catch { case _: NullPointerException => }

  def main(args: Array[String]) {
    check(monitor(null))
  }
}
