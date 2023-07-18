//> using options -Wconf:cat=other-pure-statement:silent

object Test extends App {
  def foo(p: => Unit)(x: Int = 0) = x

  assert(foo { val List(_*)=List(0); 42 } () == 0)
  assert(foo { val List(_*)=List(0); 42 } (1) == 1)
}
