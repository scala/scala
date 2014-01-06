object A {
  def unapply(n: Int): Option[Int] = Some(n)

  def run = (0: Short) match {
    case A(_) =>
    case _    =>
  }
}


object Test extends App {
  A.run
}
