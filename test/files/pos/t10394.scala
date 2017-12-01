
trait T {
  def f = for (i: Int <- List(42)) yield i
}
