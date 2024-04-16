
//> using options -no-specialization

object Test {
  // val fixesCompile = Array(1, 2, 3)

  def foo: Any = new Array[Byte](0)

  def f[@specialized(Int) A](a: A): A = a
}
