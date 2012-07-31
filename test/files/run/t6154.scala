object Test {
  def foo(a: Int) {
    var bar: Int = 0
    bar = try { 0 } catch { case ex: Throwable => 0 }
    new { foo(bar) }
  }

  def main(args: Array[String]): Unit =
    try foo(0) catch { case _: java.lang.StackOverflowError => println("ok") }
}
