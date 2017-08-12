class B extends A { override def f = 2 }
trait T extends A
class C1 extends B with T {
  def t1 = super[T].f
  def t2 = super[B].f
  def t3 = super.f
}


trait U1 extends A
trait U2 extends A
class C2 extends U1 with U2 { def t = super.f }

object Test extends App {
  val c1 = new C1
  assert(c1.t1 == 1)
  assert(c1.t2 == 2)
  assert(c1.t3 == 2)


  val c2 = new C2
  assert(c2.f == 1)
  assert(c2.t == 1)
}
