// scalac: -Xsource:3-cross

package object a {
  implicit val aOrd: Ordering[A] = null
  implicit val bOrd: Ordering[b.B] = null
}

package a {
  class A

  package aa {
    class U {
      // implicit is in an enclosing package of the callsite, not in the path of the implicit's type
      def f(xs: List[a.A]) = xs.sorted // ok
      def g(xs: List[b.B]) = xs.sorted // ok
    }
  }
}

package b {
  class B

  class V {
    def f(xs: List[a.A]) = xs.sorted // not found
  }
}

package c {
  import a._

  class W {
    def f(xs: List[a.A]) = xs.sorted // ok
    def g(xs: List[b.B]) = xs.sorted // ok
  }
}

