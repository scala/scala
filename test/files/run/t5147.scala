class Foo extends Enumeration {
  val A = Value
}
object Bar extends Foo

object Test {
  def main(args: Array[String]): Unit = {
    println(Bar.A)
  }
}
