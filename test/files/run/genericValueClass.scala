final class ArrowAssoc[A](val __leftOfArrow: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
  def →[B](y: B): Tuple2[A, B] = ->(y)
}

object Test extends App {
  {
  @inline implicit def any2ArrowAssoc[A](x: A): ArrowAssoc[A] = new ArrowAssoc(x)
  val x = 1 -> "abc"
  println(x)
  }

  {
    val y = 2 -> "def"
    println(y)
  }
}
