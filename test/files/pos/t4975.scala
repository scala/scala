object ImplicitScope {
  class A[T]

  def foo {
    trait B
    object B {
      implicit def ab = new A[B]
    }

    implicitly[A[B]]  // Error
  }
}
