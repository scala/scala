trait Base[+T]
trait Sub[+T] extends Base[T]
class SubSub[+T] extends Sub[T]

class S {
  def s[T](x: Base[_ <: T]): T = ???
  def s[U](x: Sub[_ <: U]): U = ???

  def subsub: Sub[Int] = ???

  s[Int](subsub) // ok
  s(subsub)      // fails (as expected, see comments on SI-7209)
}
