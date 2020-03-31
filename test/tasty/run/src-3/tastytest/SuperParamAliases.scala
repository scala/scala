package tastytest

object SuperParamAliases {

  trait Foo {
    def parent: Option[Foo]
    def getParent: Option[Foo] = parent
  }

  class Bar(val parent: Option[Foo]) extends Foo

}
