package q {
  class B extends p.A {
    override protected def f() { }
  }
}

package p {
  object T {
    val a = new A()
    a.f()
  }
}
