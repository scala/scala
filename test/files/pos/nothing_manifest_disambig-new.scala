import scala.reflect.runtime.universe._

object Test {
  def mani[T: TypeTag](xs: T) = xs
  mani(List())

  def listElMani[T: TypeTag](xs: List[T]) = xs
  listElMani(List())

  def foo[A, C](m : C)(implicit ev: C <:< Traversable[A], mani: TypeTag[A]): (C, A, TypeTag[A]) = (m, m.head, mani)
  foo(List(1,2,3))
}