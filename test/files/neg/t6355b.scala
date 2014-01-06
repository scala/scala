import scala.language.dynamics

class A extends Dynamic {
  def selectDynamic(method: String): B = new B(method)
}
class B(method: String) {
  def apply(x: Int) = s"$method(x: Int) called with x = $x"
  def apply(x: String) = s"""$method(x: String) called with x = "$x""""
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new A
    println(x.bippy(42))
    println(x.bippy("42"))
  }
}
