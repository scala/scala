class Before {
  def x = 1
}
object Use {
  def f = new Before()./*!*/x
  implicit class RichInt(i: Int) { def twice = i * 2 }
  def g = 3.twice
}
