class Foo(val d: Double) extends AnyVal {
  override def toString = s"Foo($d)"
}
object Test {
  def main(args: Array[String]): Unit = {
    val d: Double = null.asInstanceOf[Double]
    println(d)
    val f: Foo = null.asInstanceOf[Foo]
    println(f)
  }
}
