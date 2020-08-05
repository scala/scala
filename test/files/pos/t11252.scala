final case class Ttl(duration: Int, other: Boolean)

object Ttl {
  def apply(duration: Int) = new Ttl(duration, false)

  def unapply(x: Ttl): Option[Int] = if (x eq null) None else Some(x.duration)
}

object Test {
  def main(args: Array[String]): Unit = {
    Ttl(1) match { case Ttl(y) => println(y) }
  }
}
