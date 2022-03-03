object Test extends App {
  val xs = List(4, 5, 6)
  val ys = List(1, 2, 3, xs: _*)      // error alignment with 1 param
  def mkList1(x: Int) = List(x)
  def mkList2(x: Boolean) = List(x)
  mkList1(xs: _*)                     // error not varargs


  def f(x: Int*) = List(x: _*)

  def f(x: Boolean, y: Int*) = List(y: _*)

  def g[a](x: a*) = List(x: _*)

  f(true, 1, xs: _*)                  // error alignment with many params
  g(1, xs: _*)                        // error alignment with 1 param

  f(true, xs: _*, 17)                 // error alignment with many params, not last

  val zs = xs: _*                     // error not even close

  val txt = s"${List(42): _*}"

  implicit class summer(val sc: StringContext) {
    def sum(xs: Int*) = xs.sum.toString
  }
  val summed = sum"${List(42): _*}"

}
