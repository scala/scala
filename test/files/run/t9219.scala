object Test extends App {
  def check[U](f: Stream[Int] => U) = {
    val s = Stream.from(1)
    f(s)
    println(s)
  }

  check(_.tail)
  check(_.take(4).force)
  check(_(5))
}
