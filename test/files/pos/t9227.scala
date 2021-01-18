trait TC[M[_], @specialized(Int) A]

object Test {
  def f2[M[_], @specialized(Int) A](implicit ev: TC[M, A]): M[A] = ???
  def f1[M[_], @specialized(Int) A](implicit ev: TC[M, A]): M[A] = f2[M, A](ev)
}
