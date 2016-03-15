
trait T {
  def f = 42 match { case `x` @ _ => x }
  def g = 42 match { case `type` @ _ => `type` }
  def h = 42 match { case `type` : Int => `type` }
  def i = (null: Any) match { case _: Int | _: String => 17 }

  // arbitrary idents allowed in @ syntax
  def j = "Fred" match { case Name @ (_: String) => Name }
  def k = "Fred" match { case * @ (_: String) => * }

  // also in sequence pattern
  def m = List(1,2,3,4,5) match { case List(1, `Rest of them` @ _*) => `Rest of them` }

}
