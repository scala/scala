trait Top[@specialized(Int) A] {
  def foo(x: A): Boolean = true
}

trait Mid[@specialized(Int) A] extends Top[A] {
  override def foo(x: A): Boolean = super.foo(x)
}

class C extends Mid[Int]

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    println(c.foo(42))
  }
}
