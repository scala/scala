package scala {

  object a {

    object b {

      trait c {}
      def foo(x: c): c = bar(x)

    }

    def bar(x: b.c): a.b.c = { b.foo(x); x }
  }
}
