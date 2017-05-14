object Foo { def eq(x: Int) = x }

class X { def ==(other: String) = other.nonEmpty }

object Test {
  def main(args: Array[String]): Unit = {
    Foo eq 1
    new X == null
  }
}
