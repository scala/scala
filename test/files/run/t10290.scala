trait A1 {
  private val s = "A1"
  def f = s
}

trait A2 {
  private val s = "A2"
  def f = s
}

class B extends A1 with A2 {
  override def f = "B"
  class C {
    def t1 = B.super[A1].f
    def t2 = B.super[A2].f
    def t3 = B.this.f
  }
}

object Test {
  def main(args : Array[String]) : Unit = {
    val b = new B
    val c = new b.C
    assert(c.t1 == "A1")
    assert(c.t2 == "A2")
    assert(c.t3 == "B")
  }
}
