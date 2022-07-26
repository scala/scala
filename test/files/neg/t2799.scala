// scalac: -Xlint -Werror

@deprecated("other mother", "")
trait T[A]

// warn even though parameter is synthetic
class C[A: T] {
  def f = (t: T[A]) => null.asInstanceOf[T[A]]
  def g() = implicitly[Int => Float]
}
