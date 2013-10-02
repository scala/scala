
class NameDefaults {
  val someString = "abc"
  val someInt = 42

  def foo(x: String, y: Int)(implicit logger: Int): Int = y

  implicit val l = 42

  def bar {
    println()
    val someOtherInt = 10

    foo(y = someOtherInt/*#*/, x = someString/*#*/)
  }
}
