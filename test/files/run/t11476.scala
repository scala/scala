case class Foo(a: Int*) { def lengthCompare(i: Int) = 1 }

object Test {
  def main(args: Array[String]): Unit = {
     val x = Foo(1) match { case Foo(a) => a }
  }
}
