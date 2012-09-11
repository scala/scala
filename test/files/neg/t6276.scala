object Test {
  def foo(a: Int, b: Int, c: Int) {
    new {
      def a: Any = a // warn
      val b: Any = b // warn
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
  }
}
