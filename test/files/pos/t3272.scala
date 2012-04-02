trait A {
  trait C[+T] {
    protected[this] def f(t: T) {}
  }
  trait D[T] extends C[T] {
    def g(t: T) { f(t) }
  }
}
