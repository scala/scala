object t4606 {
  class A(var x: Int)
  class B(x: Int) extends A(x)
  trait C { self: B => 
    def foo = x 
    def bar = self.x 
    def baz = {
      val b: B = self
      b.x
    }
  }

  object Toto extends App {
    val x = new B(10) with C
    println(x.foo) // 10
    println(x.bar) // 10
    println(x.baz) // 10
    println(x.x) // 10
  }
}

object t3194 {
  class A(var x: Int)
  class B(x: Int) extends A(x) {
    self: A =>

    def update(z: Int) = this.x = z
  }
}