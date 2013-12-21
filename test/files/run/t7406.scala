class Arne[@specialized(Long) T](x: T) {
  val regularVal = x
  lazy val lazyVal = x

  def apply(f: (T, T) => T): T = f(regularVal, lazyVal)
}

object Test {
  val arne = new Arne(5L)
  def f = arne(_ + _)
  def main(args: Array[String]): Unit = {
    println(f)
  }
}
