object A  {
  def g(c: PartialFunction[Any,Unit]) {}

  def f {
    lazy val x = 0
    g { case `x` => }
  }
}
