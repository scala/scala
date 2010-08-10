object Test {
  def mani[T: Manifest](xs: T) = xs
  mani(List())

  def listElMani[T: Manifest](xs: List[T]) = xs
  listElMani(List())

  def foo[A, C](m : C)(implicit ev: C <:< Traversable[A], mani: Manifest[A]): (C, A, Manifest[A]) = (m, m.head, mani)
  foo(List(1,2,3))
}