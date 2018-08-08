
package top {
  package middle {
    class C {
      def c() = println("hello, world")
    }
    import Predef.{Map => _}
    object Test {
      def f() = Map("hello" -> "world")
      def g() = println(f())
    }
  }
}
