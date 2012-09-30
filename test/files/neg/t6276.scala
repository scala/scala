object Test {
  def foo(a: Int, b: Int, c: Int) {
    class C {
      def a: Any = a // warn
      val b: Any = b // warn

      def c: Any = this.c // warn
      def d: Any = C.this.d // warn
    }

    def method {
      // method local
      def a: Any = a // warn
    }

    trait T {
      def a: Any
    }

    new T {
      // inherited return type
      def a = a // warn
    }

    // no warnings below
    new {
      def a: Any = {println(""); a}
      val b: Any = {println(""); b}
      def c(i: Int): Any = c(i - 0)
    }

    class D {
      def other: D = null
      def foo: Any = other.foo
    }

    class E {
      def foo: Any = 0
      class D extends E {
        override def foo: Any = E.this.foo
      }
    }
  }
}
