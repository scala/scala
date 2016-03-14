case class A private (x: Int)
case class B private (x: Int)(y: Int)

class C {
  def f = A(1)
  def g = B(1)(2) // was: constructor B in class B cannot be accessed in class C
}

object Test {
  def main(args: Array[String]): Unit = {
    new C().f
    new C().g
  }

}
