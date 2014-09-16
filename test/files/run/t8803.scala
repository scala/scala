class A {
  def m = "a"
  protected def n = "a"
}

trait B {
  def m = "b"
  protected def n = "b"
}

class C extends A with B {
  override def m = "c"
  override protected def n = "c"

  val f1 = () => super[A].m
  val f2 = () => super[B].m
  val f3 = () => super.m
  val f4 = () => this.m

  val g1 = new runtime.AbstractFunction0[String] { def apply() = C.super[A].m }
  val g2 = new runtime.AbstractFunction0[String] { def apply() = C.super[B].m }
  val g3 = new runtime.AbstractFunction0[String] { def apply() = C.super.m }
  val g4 = new runtime.AbstractFunction0[String] { def apply() = C.this.m }

  val h1 = () => super[A].n
  val h2 = () => super[B].n
  val h3 = () => super.n
  val h4 = () => this.n

  val i1 = new runtime.AbstractFunction0[String] { def apply() = C.super[A].n }
  val i2 = new runtime.AbstractFunction0[String] { def apply() = C.super[B].n }
  val i3 = new runtime.AbstractFunction0[String] { def apply() = C.super.n }
  val i4 = new runtime.AbstractFunction0[String] { def apply() = C.this.n }
}

object Test extends App {
  val c = new C
  println(c.f1())
  println(c.f2())
  println(c.f3())
  println(c.f4())

  println(c.g1())
  println(c.g2())
  println(c.g3())
  println(c.g4())

  println(c.h1())
  println(c.h2())
  println(c.h3())
  println(c.h4())

  println(c.i1())
  println(c.i2())
  println(c.i3())
  println(c.i4())
}
