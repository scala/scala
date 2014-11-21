class Wrapper[X](x: X)

class C {
  def a(w: Wrapper[Array[Int]]) = 0
  def b(w: Wrapper[Int]) = 0
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    c.a(new Wrapper(Array(1, 2)))
    c.b(new Wrapper(1))

    val methods = classOf[C].getDeclaredMethods.sortBy(_.getName)
    println("= Java Erased Signatures =")
    println(methods.mkString("\n"))
    println("\n= Java Generic Signatures =")
    println(methods.map(_.toGenericString).mkString("\n"))
  }
}
