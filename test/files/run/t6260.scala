class Box[X <: CharSequence](val x: X) extends AnyVal {
  def map[Y <: CharSequence](f: X => Y): Box[Y] =
    ((bx: Box[X]) => new Box(f(bx.x)))(this)
  override def toString = s"Box($x)"
}

object Test {
  def main(args: Array[String]) {
    val g = (x: String) => x + x
    println(new Box("abc") map g)
  }
}
