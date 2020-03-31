package tastytest

object TestSuperParamAliases extends Suite("SuperParamAliases") {

  class Qux(parent: Option[Qux]) extends SuperParamAliases.Bar(parent)

  test(assert(new Qux(None).getParent === Option.empty[SuperParamAliases.Foo]))

}
