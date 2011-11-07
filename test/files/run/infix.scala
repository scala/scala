case class op(x: op, y: Int, z: Int) {
  def op(y: Int, z: Int) = new op(this, y, z)
}

object Test extends App {
  val xs = new op(null, 0, 0) op (1, 1) op (2, 2)
  Console.println(xs)
  xs match {
    case null op (0, 0) op (1, 1) op (2, 2) => Console.println("OK")
  }
}
  
