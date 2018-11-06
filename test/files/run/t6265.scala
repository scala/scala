object Test {
  trait B[@specialized(Int) T] {
    def foo(x: T) = println("B.foo " + x)
  }

  trait M[@specialized(Int) T] extends B[T] {
    abstract override def foo(x: T) = {
      super.foo(x)
      println("M.foo " + x)
    }
  }

  object C extends B[Int] with M[Int]

  def main(args: Array[String]): Unit =
    println(C.foo(1))
}
