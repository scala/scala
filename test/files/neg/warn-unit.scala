
@scala.annotation.compileTimeOnly("please don't")
object X extends Specializable

class C[@specialized(X, Int) A] {
  def f(a: A) = 42
}

object Main {
  def main(args: Array[String]): Unit = {
    val c = new C[Int]
    println(c.f(42))
    println(X)
    Unit
  }
}
