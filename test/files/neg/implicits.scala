class Pos

class Super

object Super {
  implicit def pos2int(p: Pos): int = 0
}

object Sub extends Super {
  class Plus(x: Any) {
    def +(y: String): String = x.toString + y
  }
  implicit def any2plus(x: Any): Plus = new Plus(x)
}

object Test {
  import Super._
  import Sub._
  val p = new Pos
  def f(x: int): int = x
  f(p+1)
}
