object Test
{
  def s1 = "bob".toList  match { case Seq('b', 'o', 'b') => true }  // list ok

  // not final, allowed
  class Bop
  def s2(x: Bop) = x match { case Seq('b', 'o', 'b') => true }

  // covariance, allowed
  final class Bop4[+T]
  def s3[T](x: Bop4[T]) = x match { case Seq('b', 'o', 'b') => true }

  // contravariance, allowed
  final class Bop5[T, U, -V]
  def s4[T1, T2](x: Bop5[_, T1, T2]) = x match { case Seq('b', 'o', 'b') => true }

  // free type parameter, allowed
  final class Bop3[T]
  def f4[T](x: Bop3[T]) = x match { case Seq('b', 'o', 'b') => true }

  // String and Array are final/invariant, disallowed
  def f1 = "bob".reverse match { case Seq('b', 'o', 'b') => true } // fail
  def f2 = "bob".toArray match { case Seq('b', 'o', 'b') => true } // fail

  // final, no type parameters, should be disallowed
  final class Bop2
  def f3(x: Bop2) = x match { case Seq('b', 'o', 'b') => true } // fail

  // final, invariant type parameter, should be disallowed
  def f4[T](x: Bop3[Char]) = x match { case Seq('b', 'o', 'b') => true } // fail
}
