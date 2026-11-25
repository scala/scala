// The outer accessor that an anonymous class created in a secondary constructor gets for an
// inner trait it mixes in must refer to the outer instance of the anonymous class, not to the
// outer parameter of the enclosing constructor.

// The trait is a member of the class whose constructor creates the anonymous class.
class O1 {
  class I {
    trait T { def i: I = I.this }
    var t: T = _
    def this(a: Int) = { this(); t = new T {} }
  }
}

// The trait is a member of the outer class.
class O2 {
  val name = "O2"
  trait T { def who = name }
  class I { var t: T = _; def this(a: Int) = { this(); t = new T {} } }
}

// One more level of nesting.
class O3 {
  val name = "O3"
  class Outer {
    val name = "O3.Outer"
    trait T { def who = name }
    class I { var t: T = _; def this(a: Int) = { this(); t = new T {} } }
  }
}

// The wrong outer instance has the right class, so a cast would not catch it.
class O4 {
  val name = "O4"
  trait T { def who = name; def self: O4 = O4.this }
  class I extends O4 {
    override val name = "O4.I"
    var t: T = _
    def this(a: Int) = { this(); t = new T {} }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val o1 = new O1
    val i1 = new o1.I(1)
    assert(i1.t.i eq i1)

    val o2 = new O2
    println(new o2.I(1).t.who)

    val o3 = new O3
    val o3o = new o3.Outer
    println(new o3o.I(1).t.who)

    val o4 = new O4
    val i4 = new o4.I(1)
    println(i4.t.who)
    assert(i4.t.self eq i4)
  }
}
