class A {
  class C extends { val x: A = this } with AnyRef
}

class B(x: Int)

class D {
  class C(x: Int) extends B({val test: D = this; x}) {
    def this() {
      this({val test: D = this; 1})
    }
  }
}
