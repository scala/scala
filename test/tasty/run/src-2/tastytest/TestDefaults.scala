package tastytest

object TestDefaults extends Suite("TestDefaults") {

  test("new Defaults()()") {
    val defaults = new Defaults()()
    assert(defaults.i == 33)
    assert(defaults.s == "foo")
    assert(defaults.b == false)
  }

  test("""new Defaults(s="bar")()""") {
    val defaults = new Defaults(s="bar")()
    assert(defaults.i == 33)
    assert(defaults.s == "bar")
    assert(defaults.b == false)
  }

  test("""new Defaults(s="baz", i=21)()""") {
    val defaults = new Defaults(s="baz", i=21)()
    assert(defaults.i == 21)
    assert(defaults.s == "baz")
    assert(defaults.b == false)
  }

  test("""new Defaults(s="qux", i=93)(b=true)""") {
    val defaults = new Defaults(s="qux", i=93)(b=true)
    assert(defaults.i == 93)
    assert(defaults.s == "qux")
    assert(defaults.b == true)
  }

  test("""new Defaults(101, "bip")(true)""") {
    val defaults = new Defaults(101, "bip")(true)
    assert(defaults.i == 101)
    assert(defaults.s == "bip")
    assert(defaults.b == true)
  }

  test("""Defaults(101, "bip")(true)""") {
    val defaults = Defaults(101, "bip")(true)
    assert(defaults.i == 101)
    assert(defaults.s == "bip")
    assert(defaults.b == true)
  }

  test("""new Defaults()().foo()()() = (0,"",false)""") {
    val foo = new Defaults()().foo()()()
    assert(foo === (0,"",false))
  }

  test("""new Defaults()().foo()(b="wow")() = (0,"wow",false)""") {
    val foo = new Defaults()().foo()(b="wow")()
    assert(foo === (0,"wow",false))
  }

  test("""new Defaults.Specialised().bar() = (12L,"abc",true)""") {
    val bar = new Defaults.Specialised().bar()
    assert(bar === (12L,"abc",true))
  }

  test("""new Defaults.OutOfOrder().bar() = (12L,"abc",true)""") {
    val bar = new Defaults.OutOfOrder().bar()
    assert(bar === (12L,"abc",true))
  }

}
