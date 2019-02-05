object ImplicitScope {
  class A[T]

  def foo: Unit = {
    trait B
    object B {
      implicit def ab = new A[B]
    }

    implicitly[A[B]]  // Error
  }
}
