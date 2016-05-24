object T9745 {
  var i = 0
  def f(i: Unit)(j: Int): Int = ???
  val g = x => f(i += 1)(x)
}
