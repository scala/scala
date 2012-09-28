
sealed trait A[T]
final class B[T] extends A[T]

object ParsedAxis {
  type BI = B[Int]

  def f1(a: A[Int]) = a match { case b: B[Int] => 3 }
  def f2(a: A[Int]) = a match { case b: BI => 3 }
  def f3(a: A[Int]) = a match { case b: B[t] => 3 }
}
