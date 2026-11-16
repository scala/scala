
package top {
  package middle {
    import bottom.B.b
    class C {
      def p() = println("hello, world")
      def f = b     // completes b before the typer reaches the import that masks Set
    }
  }
}

package bottom {
  import Predef.{Set => _}
  object B {
    def b = Set(42)
  }
}
