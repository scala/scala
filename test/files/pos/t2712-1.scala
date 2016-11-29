package test

// Original test case from,
//
//   https://issues.scala-lang.org/browse/SI-2712
object Test {
  def meh[M[_], A](x: M[A]): M[A] = x
  meh{(x: Int) => x} // solves ?M = [X] Int => X and ?A = Int ...
}
