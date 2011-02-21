object Test extends App {
 trait A
 trait B extends A

  trait C {
    type U
    trait D { type T >: B <: A }
    val y: (D with U)#T = new B { }
  }

  class D extends C {
    trait E
    trait F { type T = E }
    type U = F
    def frob(arg : E) : E = arg
    frob(y)
  }

  new D
}

