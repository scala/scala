
package top {
  package middle {
    import bottom.B.b
    class C {
      def p() = println("hello, world")
      def f = b     // comment me to see Predef exclusion
    }
  }
}

package bottom {
  import Predef.{Set => _}
  object B {
    def b = Set(42)
  }
}
