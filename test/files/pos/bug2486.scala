class A[T]
class B extends A[Int]
class C[T] extends A[T] { def f(t: A[T]) = t match { case x: B => () } }
