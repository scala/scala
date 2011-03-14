
trait A {
  def f() = println("A")
}

class B extends A {
  def b() = super[A].f()
  trait C {
    def c() = B.super[A].f()
  }
  def g() = for(i <- 0 until 1) super[A].f()
  def h() = for(i <- 0 until 1) B.super[A].f()
  override def f() = println("B")
}


object Test {
  def main(args : Array[String]) : Unit = {
    val b = new B()
    b.b()
    new b.C(){}.c()
    b.g() // was ClassCastException
    b.h() // was ClassCastException
  }
}
