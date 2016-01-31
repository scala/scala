
trait T {
  def f = 42 match { case `x` @ _ => x }
  def g = 42 match { case `type` @ _ => `type` }
  def h = 42 match { case `type` : Int => `type` }
  def i = (null: Any) match { case _: Int | _: String => 17 }
}
