//> using options -Xsource:3
//

class D {
  def *(y: Int): Int = y
  def unrelated(y: Int): Int = y
}

object Test {
  val d = new D

  def one: Int = {
    import d.`*`

    unrelated(1) // error

    *(1)
  }

  def two: Int = {
    import d.*

    unrelated(1)

    *(1)
  }
}

trait T[A] {
  def t: A
}
object T {
  implicit def tInt: T[Int] = new T[Int] {
    def t: Int = 42
  }
  def f[A](implicit t: T[A]): A = t.t
}
object X {
  import T.given
  def g = f[Int] // error no f, was given is not a member
}
