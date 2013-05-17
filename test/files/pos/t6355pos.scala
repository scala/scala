import scala.language.dynamics

class A extends Dynamic {
  def applyDynamic[T1](method: String)(x1: T1): Any = 1
  def applyDynamic[T1, T2](method: String)(x: T1, y: T2): Any = 2
  def applyDynamic[T1, T2, T3](method: String)(x: T1, y: T2, z: T3): Any = 3
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new A
    println(x[Int](5))
    println(x[Int, String](5, "a"))
    println(x[Int, String, Int](5, "a", 5))
  }
}
