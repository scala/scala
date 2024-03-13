//> using options -Xsource:3 -Werror

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
    def f(xs: List[a.A]) = xs.sorted // warn
  }
}

package c {
  import a._

  class W {
    def f(xs: List[a.A]) = xs.sorted // ok
    def g(xs: List[b.B]) = xs.sorted // ok
  }
}

package a1 {

  package object a2 {
    implicit def myClassToSeq[A](a: MyClass[A]): Seq[A] = a.values
  }

  package a2 {
    case class MyClass[A](values: Seq[A])
  }

  object Main {
    def f[A](x: Seq[a1.a2.MyClass[A]]): Seq[A] = x.flatten
  }
}
