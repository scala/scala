object Test {
  trait A[T[C], @specialized(Int) C] {
    def f(x: T[C]): T[C]
  }

  trait B[T[C], @specialized(Int) C] extends A[T, C] {
    def f(x: T[C]) = x
  }
}
