object A
trait B[T]

object C {
  implicit def notUsed[L[x]](in: L[Int]): B[L[Int]] = ???

  class E(val ls: Int) {
    def x(f: Int => Boolean): Boolean = f(ls)
  }
  implicit def isUsed(ls: Int): E = new E(ls)

  def amethod(in: Int): Boolean =
    in.x { i =>
      import A._
      "asdf" == i.toString
    }
}