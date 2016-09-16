object t1 {
  trait T { def f = 1 }
  trait U extends T
  class C extends U { def t = super.f }
}

object t2 {
  class A { def f = 1 }
  trait T extends A { override def f = 2 }
  class B extends A
  class C extends B with T {
    def t1 = super.f
    def t2 = super[T].f
    def t3 = super[B].f
  }
}

object t3 {
  class A { def f = 1 }
  trait T extends A
  class B extends A { override def f = 2 }
  class C extends B with T {
    def t1 = super.f
    // def t2 = super[T].f // error: cannot emit super call (test exists)
    def t3 = super[B].f
  }
}

object t4 {
  trait T1 { def f = 1 }
  trait T2 { self: T1 => override def f = 2 }
  trait U extends T1 with T2
  class C extends U {
    def t1 = super.f
    def t2 = super[U].f
  }
}

object t5 {
  trait T { override def hashCode = -1 }
  trait U extends T
  class C extends U {
    def t1 = super[U].hashCode
    def t2 = super.hashCode
  }
}

object t6 {
  trait T { def f = 1 }
  trait U1 extends T { override def f = 2 }
  trait U2 extends T { override def f = 3 }
  class C1 extends T with U1 with U2 {
    def t1 = super.f
    def t2 = super[T].f
    def t3 = super[U1].f
    def t4 = super[U2].f
  }
  class C2 extends T with U2 with U1 {
    def t1 = super.f
  }
}

object t7 {
  trait T1 { def f = 1 }
  trait T2 { _: T1 => override def f = 2 }
  trait U extends T1 with T2
  trait V extends U with T2
  class C extends V {
    def t1 = super.f
    def t2 = super[V].f
  }
}

object t8 {
  trait HasNewBuilder { def newBuilder: Int }
  trait GenericTraversableTemplate extends HasNewBuilder { def newBuilder = 0 }
  trait Iterable extends GenericTraversableTemplate
  trait MutMapLike extends HasNewBuilder { override def newBuilder = 1 }
  trait MutMap extends Iterable with MutMapLike
  class TrieMap extends MutMap with MutMapLike
}

object Test {
  def e(a: Any, b: Any) = assert(a == b, s"expected: $b\ngot: $a")

  def main(args: Array[String]): Unit = {
    e(new t1.C().t, 1)

    val c2 = new t2.C
    e(c2.f, 2)
    e(c2.t1, 2)
    e(c2.t2, 2)
    e(c2.t3, 1)

    val c3 = new t3.C
    e(c3.f, 2)
    e(c3.t1, 2)
    e(c3.t3, 2)

    val c4 = new t4.C
    e(c4.f, 2)
    e(c4.t1, 2)
    e(c4.t2, 2)

    val c5 = new t5.C
    e(c5.hashCode, -1)
    e(c5.t1, -1)
    e(c5.t2, -1)

    val c6a = new t6.C1
    val c6b = new t6.C2
    e(c6a.f, 3)
    e(c6a.t1, 3)
    e(c6a.t2, 1)
    e(c6a.t3, 2)
    e(c6a.t4, 3)
    e(c6b.f, 2)
    e(c6b.t1, 2)

    val c7 = new t7.C
    e(c7.f, 2)
    e(c7.t1, 2)
    e(c7.t2, 2)

    e(new t8.TrieMap().newBuilder, 1)
  }
}
