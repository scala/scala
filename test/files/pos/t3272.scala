trait A {
  trait C[+T] {
    protected[this] def f(t: T): Unit = {}
  }
  trait D[T] extends C[T] {
    def g(t: T): Unit = { f(t) }
  }
}
