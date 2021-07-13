class B[@specialized(Int) T](t: T) {
  val a = t
  val b = "?"
}

object Test {
  def main(args: Array[String]): Unit =
    println(new B(42).b)
}
