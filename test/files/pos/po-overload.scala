package p {
  object X extends App {
    B.f(0) // ok
    q.f(0) // nope
  }

  trait A {
    def f(x: String): String
  }

  package q {
    object `package` extends A {
      def f(x: Int): String = "po q int"
      override def f(x: String): String = "po q str"
    }
    class C {
      def g = f(42)
    }
  }

  object B extends A {
    def f(x: Int): String    = ???
    override def f(x: String): String = ???
  }
}
package p2 {
  object X extends App {
    B.f(0) // ok
    q.f(0) // nope
  }

  trait A {
    def f(x: String): String = "A"
  }

  package q {
    object `package` extends A {
      def f(x: Int): String = "po q int"
      override def f(x: String): String = "po q str"
    }
    class C {
      def g = f(42)
    }
  }

  object B extends A {
    def f(x: Int): String    = ???
    override def f(x: String): String = ???
  }
}
package p3 {
  trait A {
    class C
    //type C
    //object C
    val pi = 3.14
  }
  package q {
    object `package` extends A {
      class C // warning: shadowing a nested class of a parent is deprecated
    }
    class D {
      def c = new C
      def x = pi
    }
  }
}
package p4 {
  object X {
    q.f(0)
  }

  trait A {
    def f(x: String): String = "A"
  }
  trait B extends A {
    def pi = 3.14
  }

  package q {
    object `package` extends B {
      def f(x: Int): String = "po q int"
      override def f(x: String): String = "po q str"
    }
    class C {
      def g = f(42)
      def x = pi
    }
  }
}
