//> using options -Werror -Wunused:_
class Private {
  private type Curry[A] = { type T[B] = Either[A, B] }
  def m2[T[A]]: Unit = ()
  def f() = m2[Curry[Int]#T]
}
