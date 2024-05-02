//> using options -Werror -Xlint
case class C(i: Int)(j: => Int)(k: => Int) { def sum = i + j + k }

object Test extends App {
  println {
    C(42)(27)(1).copy()
  }
}
