package example

class A {
  case class B(x: Int)
  def c = B
}
object A {
  def main(args: Array[String]): Unit = {
    (new A).c
  }
}
