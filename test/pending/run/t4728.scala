class X
class Y extends X
object Ambiguous {
  def f(x: X) = 1
  def f(ys: Y*) = 2
}

object Test extends App {
  println(Ambiguous.f(new X))
  println(Ambiguous.f(new Y))
}