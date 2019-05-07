class B[A]
sealed trait T[A] {
  def overloaded(that: List[T[A]]): T[A] = that.head
  def overloaded(that: List[B[A]]): B[A] = that.head
}
abstract class C[A] extends T[A]
