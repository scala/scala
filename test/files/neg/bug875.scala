object Test extends Application {
  val xs = List(4, 5, 6)
  val ys = List(1, 2, 3, xs: _*)
  def mkList(x: Int) = List(x)
  def mkList(x: Boolean) = List(x)
  mkList(xs: _*)


  def f(x: Int*) = List(x: _*)

  def f(x: Boolean, y: Int*) = List(y: _*)

  def g[a](x: a*) = List(x: _*)

  f(true, 1, xs: _*)
  g(1, xs:_*)

}
