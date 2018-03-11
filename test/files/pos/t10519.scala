object Test {
  case class D[A]()
  val xs = Seq(D[Int](), D[Boolean]())
  def g[Y](is: Seq[D[_ >: Y]]) = ???
  g(xs)
}
