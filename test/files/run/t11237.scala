// using options -Vprint:~uncurry

trait Semigroup[F] { self =>
  def append(f1: F, f2: => F): F
  val z = 10
}

case class Box(i: Int)

class R extends Runnable {
  def run() = {
    val boxSemigroup: Semigroup[Box] = (x1, x2) => Box(x1.i + x2.i)
    //val boxSemigroup: Semigroup[Box] = (x1: Box, x2: Box) => Box(Math.max(x1.i, x2.i)) // disallowed, by-name must be inferred
    assert(boxSemigroup.append(Box(1), Box(2)) == Box(3))
  }
}

object Test extends App {
  new R().run()
}
