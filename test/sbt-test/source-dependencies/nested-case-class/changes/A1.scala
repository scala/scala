package example

class VC(val self: Int) extends AnyVal

class A {
  case class B(x: VC)
  def c = B
}
object A {
  def main(args: Array[String]): Unit = {
    (new A).c
  }
}