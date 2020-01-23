package tastytest

object TestDefaults extends Suite("TestDefaults") {

  test("""new Defaults.OutOfOrder().bar() = (12L,"abc",true)""") {
    val bar = new Defaults.OutOfOrder().bar()
    assert(bar === (12L,"abc",true))
  }

}
