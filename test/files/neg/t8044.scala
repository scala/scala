
trait T {
  def f = 42 match { case `_` : Int => `_` }   // doesn't leak quoted underscore
}
