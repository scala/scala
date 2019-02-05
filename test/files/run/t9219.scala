object Test extends App {
  def check[U](f: LazyList[Int] => U) = {
    val s = LazyList.from(1)
    f(s)
    println(s)
  }

  check(_.tail)
  check(_.take(4).force)
  check(_(5))
}
