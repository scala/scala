package tastytest

object TestNestedEnum extends Suite("TestNestedEnum") {

  test("call toString on enum of inner class") {
    val n = new NestedEnum()
    assert(n.Mode.On.toString == "On")
  }

  test("nested enums do not have same type") {
    val n1 = new NestedEnum()
    val n2 = new NestedEnum()
    implicitly[scala.util.NotGiven[n1.Mode.Off.type =:= n2.Mode.Off.type]]
    assert(n1.Mode.Off != n2.Mode.Off)
  }

}
