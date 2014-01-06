class Foo(val d: Double) extends AnyVal {
  override def toString = s"Foo($d)"
}

class Bar(val d: String) extends AnyVal {
  override def toString = s"Foo($d)"
}

object Test {
  def main(args: Array[String]): Unit = {
    val f: Foo = {val n: Any = null; n.asInstanceOf[Foo]}
    println(f)

    val b: Bar = {val n: Any = null; n.asInstanceOf[Bar]}
    println(b)
  }
}
