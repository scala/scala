object Test extends App {
 trait A
 trait B extends A

  trait C {
    type U
    trait D { type T >: B <: A }
    type V <: D
    val y: V#T = new B { }
  }

  trait Middle extends C {
    type V <: (D with U)
  }

  class D extends Middle {
    trait E
    trait F { type T = E }
    type U = F
    def frob(arg : E) : E = arg
    frob(y)
  }

  new D
}
