object Test extends App {
 trait A
 trait B extends A

  trait C {
    trait D { type T >: B <: A }
    val y: (D with this.type)#T = new B { }
  }

  class D extends C {
    trait E
    type T = E 
    def frob(arg : E) : E = arg
    frob(y)
  }

  new D
}