class A {
  trait T[A]
  def usetHk[T[_], A](ta: T[A]) = 0
  usetHk(new T[Int]{}: T[Int])
  usetHk(new T[Int]{}) // fails with: found: java.lang.Object with T[Int], required: ?T[ ?A ]
}