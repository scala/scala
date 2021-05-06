class A { object X }

class C {
  val a, b = new A; import a.X
  class D {
    def isInstanceOf_aX(z: AnyRef) = z.isInstanceOf[X.type]
    class E {
      def isInstanceOf_aX(z: AnyRef) = z.isInstanceOf[X.type]
    }
  }
}

object Test extends C {
  def main(args: Array[String]): Unit = {
    val d = new D()
    assert(d.isInstanceOf_aX(a.X))
    assert(!d.isInstanceOf_aX(b.X))
    assert(!d.isInstanceOf_aX(new Object))

    val e = new d.E()
    assert(e.isInstanceOf_aX(a.X))
    assert(!e.isInstanceOf_aX(b.X))
    assert(!e.isInstanceOf_aX(new Object))
  }
}
