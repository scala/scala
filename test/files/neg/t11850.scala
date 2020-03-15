// scalac: -Werror -Xlint:pattern-shadow

trait T {
  val x = 42

  def f(i: Int) =
    i match {
      case `x` => 0    // presence of this case really obviates warning on the next?
      case x   => 1    // warn
    }
  def g(i: Int) =
    i match {
      case x @ _ => 1
    }
}
