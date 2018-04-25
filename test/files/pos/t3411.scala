object A  {
  def g(c: PartialFunction[Any,Unit]): Unit = {}

  def f: Unit = {
    lazy val x = 0
    g { case `x` => }
  }
}
