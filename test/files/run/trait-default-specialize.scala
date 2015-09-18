trait T[@specialized(Int) A] {
  def t(a: A): Unit
}

object Test {
  def main(args: Array[String]): Unit = {
    class TInt extends T[Int] { def t(a : Int) = println(a) }
    val tMethods = classOf[TInt].getInterfaces.head.getMethods.filter(_.getName == "t")
    println(tMethods.map(_.toString).sorted.mkString("\n"))
    new TInt().t(0)
    def call[A](t: T[A], a: A) = t.t(a)
    call[Int](new TInt(), 0)
  }
}
