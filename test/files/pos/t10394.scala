
//> using options -Xfatal-warnings -Ywarn-unused:patvars
trait T {
  def f = for (i: Int <- List(42)) yield i
}
