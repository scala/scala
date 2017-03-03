
trait T {
  def g = 42 match { case `Oops` : Int => }  // must be varish
}
