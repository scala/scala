class Foo(val bar: Double) extends AnyVal {
  def this(s: String) = this(s.toDouble)
}
object Test {
  def main(args: Array[String]): Unit =
    new Foo("")
 }

