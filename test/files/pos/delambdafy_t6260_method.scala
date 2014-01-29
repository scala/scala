class Box[X](val x: X) extends AnyVal {
  def map[Y](f: X => Y): Box[Y] =
    ((bx: Box[X]) => new Box(f(bx.x)))(this)
}

object Test {
  def map2[X, Y](self: Box[X], f: X => Y): Box[Y] =
    ((bx: Box[X]) => new Box(f(bx.x)))(self)

  def main(args: Array[String]) {
    val f = (x: Int) => x + 1
    val g = (x: String) => x + x

    map2(new Box(42), f)
    new Box("abc") map g
  }
}
