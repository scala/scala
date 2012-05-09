
class X
class Y extends X

// A lead star but no diva.
object Ambiguous {
  def f(x: X) = 1
  def f(ys: Y*) = 2
}

object Test {
  def main(args: Array[String]) {
    println(Ambiguous.f(new Y))
  }
}
