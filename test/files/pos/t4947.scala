class DependentImplicitTezt {
  trait Bridge

  class Outer {
    class Inner extends Bridge

    object Inner {
      implicit def fromOther(b: Bridge): Inner = throw new Error("todo")
    }

    def run(x: Inner) = throw new Error("todo")
  }

  val o1 = new Outer
  val o2 = new Outer
  val i1 = new o1.Inner
  val i2 = new o2.Inner

  def doesntCompile {
    o1.run(i2) // should compile
  }

  def workaround1 {
    o1.run(i2: Bridge) // ok
  }

  def workaround2 {
    import o1.Inner.fromOther
    o1.run(i2) // ok
  }
}
