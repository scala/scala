
object Test extends App {
  assert(s"a\tb" == "a\tb")
  def f = () => s"a\tb"
  assert(f() == "a\tb")
  def g(x: => String) = x
  assert(g(s"a\tb") == "a\tb")
}
