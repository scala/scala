case class Foo(x: Int, ys: Int*) {
  // We write our own toString because of SI-7735
  override def toString = (x +: ys).mkString("Foo(", ", ", ")")
}

object Test {
  def f1(x: Any) = x match {
    case Foo(x) => true
    case _      => false
  }
  def f2(x: Any) = x match {
    case Foo(x, y) => true
    case _         => false
  }
  def f3(x: Any) = x match {
    case Foo(x, y, z) => true
    case _            => false
  }
  def f1seq(x: Any) = x match {
    case Foo(x, ys @ _*) => true
    case _               => false
  }
  def f2seq(x: Any) = x match {
    case Foo(x, y, zs @ _*) => true
    case _                  => false
  }
  def f3seq(x: Any) = x match {
    case Foo(x, y, z, qs @ _*) => true
    case _                     => false
  }

  val x1 = Foo(1)
  val x2 = Foo(1, 2)
  val x3 = Foo(1, 2, 3)

  val fs = List[Any => Boolean](f1, f2, f3)
  val fseqs = List[Any => Boolean](f1seq, f2seq, f3seq)
  val xs = List[Foo](x1, x2, x3)

  def main(args: Array[String]): Unit = {
    for ((f, i) <- fs.zipWithIndex) {
      xs foreach (x => println(s"f${i+1}($x) == ${f(x)}"))
      println("")
    }
    for ((f, i) <- fseqs.zipWithIndex) {
      xs foreach (x => println(s"f${i+1}seq($x) == ${f(x)}"))
      println("")
    }
  }
}
