class TC[T[_]] {
  def identity[A](a: T[A]): T[A] = a
}
object Test {
  def value: TC[({type l1[x] = Option})#l1] = ??? // error not reported!

  type l2[x] = Option // error correctly reported
  def value1: TC[l2] = ???
}

