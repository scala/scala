object Test extends App {
  class L[A]
  class Quux0[B, CC[_]]
  class Quux[C] extends Quux0[C, L]

  def foo[D[_]](x: D[D[Boolean]]) = ???
  def bar: Quux[Int] = ???

  foo(bar)
}
