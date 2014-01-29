class C[A](private val a: Any) extends AnyVal

object Test {
  val f = (x: C[Any]) => {println(s"f($x)"); x}
  trait T[A] {
    def apply(a: A): A
  }
  val g = new T[C[Any]] { def apply(a: C[Any]) = { println(s"g($a)"); a } }
  def main(args: Array[String]) {
     f(new C("."))
     val methods = f.getClass.getDeclaredMethods.map(_.getName).sorted
     println("")
     println(methods.mkString("\n"))
     g.apply(new C("."))
  }
}

