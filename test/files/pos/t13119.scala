trait A[T]
final class C[T, E] extends A[T with E]

object T {
  def f(x: A[?]): Int = x match {
    case b: C[?, ?] => 0
    case _ => 1
  }
}
