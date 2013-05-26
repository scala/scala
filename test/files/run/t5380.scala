object Test {
  def main(args: Array[String]) {
    val f = () => return try { 1 } catch { case _: Throwable => 0 }
    f()
  }
}
