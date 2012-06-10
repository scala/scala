trait FM[A] {
  def map(f: A => Any)
}

trait M[A] extends FM[A] {
  def map(f: A => Any)
}

trait N[A] extends FM[A]

object test {
  def kaboom(xs: M[_]) = xs map (x => ()) // missing parameter type.

  def okay1[A](xs: M[A]) = xs map (x => ())
  def okay2(xs: FM[_]) = xs map (x => ())
  def okay3(xs: N[_]) = xs map (x => ())
}


// does not compile
class CC2(xs: List[_]) {
  def f(x1: Any, x2: Any) = null
  def g = xs map (x => f(x, x))
}
