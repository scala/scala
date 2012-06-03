object DepBug {
  class A {
    class B
    def mkB = new B
    def m(b : B) = b
  }

  trait Dep {
    val a : A
    val b : a.B
  }

  val dep = new Dep {
    val a = new A
    val b = a.mkB
  }

  def useDep(d : Dep) {
    import d._
    a.m(b)         // OK
  }

  {
    import dep._
    a.m(b)           // OK with 2.9.1.final, error on trunk
  }

  dep.a.m(dep.b)

}
