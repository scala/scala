abstract class Bomb {
  type T
  val x: T

  def size(that: T): Int
}

class StringBomb extends Bomb {
  type T = String
  val x = "abc"
  def size(that: String): Int = that.length
}

class IntBomb extends Bomb {
  type T = Int
  val x = 10

  def size(that: Int) = x + that
}

case class Mean(var bomb: Bomb)

object Test extends App {
  def foo(x: Mean) = x match {
    case Mean(b) =>
      // BUG: b is assumed to be a stable identifier, but it can actually be mutated
      println(b.size({ mutate(); b.x }))
  }

  def mutate() {
    m.bomb = new IntBomb
  }

  val m = Mean(new StringBomb)
  foo(m) // should print 3
}