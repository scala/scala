object typeMember {
  class Foo {
    type FT
    class I
    def m(b: FT, o: Option[I]): Int = 0
  }

  object Test {
    def f[T]: Foo { type FT = T } = ???
    def t = {
      val b: Any = ???
      f.m(b, None)
    }
  }
}

object typeParam {
  class Foo[FT] {
    class I
    def m(b: FT, o: Option[I]): Int = 0
  }

  object Test {
    def f[T]: Foo[T] = ???
    def t = {
      val b: Any = ???
      f.m(b, None)
    }
  }
}
