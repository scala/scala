package foo {
  object syntax {
    @deprecatedError("use := syntax instead", since="foo-lib 1.0")
    def <<=() = ???
  }
}

object Test1 {
  import foo.syntax._
  <<=()
}
