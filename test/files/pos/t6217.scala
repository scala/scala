//> using options -Xfatal-warnings
package p {
  package _root_ {
    package scala {
      object Option {
        def apply(b: Boolean) = if (b) "true" else "false"
      }
    }
  }
}
package p {
  object Test {
    import p._root_.scala.Option
    def f = Option(true)
  }
}
