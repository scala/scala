package test

// Original test case from,
//
//   https://github.com/scala/bug/issues/2712
object Test {
  def meh[M[_], A](x: M[A]): M[A] = x
  meh{(x: Int) => x} // solves ?M = [X] Int => X and ?A = Int ...
}
