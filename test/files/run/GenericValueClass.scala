class Box[T](val x: T) extends AnyVal {
  def get: T = x
}

object Test extends App {
  val b = new Box(1)
  println(b.get)

  val c = new Box("abc")
  println(c.get)

}
