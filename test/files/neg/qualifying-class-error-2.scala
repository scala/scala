package A {
  trait X {
    protected[A] def f(): Unit
  }
}

package B {
  class Y extends A.X {
    protected[A] def f(): Unit = {}
  }
}
