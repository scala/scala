package A {
  trait X {
    protected[A] def f()
  }
}

package B {
  class Y extends A.X {
    protected[A] def f() {}
  }
}
