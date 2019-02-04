trait A[@specialized(Int) T] {
  def f(x: T): Unit
}

trait B[@specialized(Int) T] {
  def g(x: T): Unit = {
    val frames = Thread.currentThread().getStackTrace.toList.drop(1).takeWhile(_.getMethodName != "main")
    println(frames.map(_.getMethodName).mkString("\n"))
  }
}

class C[@specialized(Int) T] extends A[T] with B[T] {
  def f(x: T): Unit = g(x)
}

object Test {
  def main(args: Array[String]): Unit = {
    new C[Int].f(0)
  }
}
