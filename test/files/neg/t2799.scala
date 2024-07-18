//> using options -Xlint -Werror

@deprecated("other mother", "")
trait T[A]

// warn even though parameter is synthetic
class C[A: T] {
  def f = (t: T[A]) => null.asInstanceOf[T[A]]
  def g() = implicitly[Int => Float]
}

@deprecated("hi mom", "")
case class Bob ()

// No exclusion for companion of deprecated T
object T extends T[String] {
  def t = Bob() // warn
}

class Client {
  def test = T.t // if no warn at t, then this code appears deprecation-free
}
