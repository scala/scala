package tastytest

object TestSuperTypes extends Suite("TestSuperTypes") {

  test("SUPER in type tree") {
    assert((new SuperTypes.Bar().bar: "Foo.foo") === "Foo.foo")
  }

  test("SUPERtype in type") {
    val bar = new SuperTypes.Bar()
    assert(("" match { case bar.A(x) => x: "Foo.foo" }) === "Foo.foo")
  }

  test("SUPERtype in type, version 2") {
    val bar = new SuperTypes.Bar()
    assert(("" match { case bar.A(x) => x : bar.foo.type }) === "Foo.foo")
  }

  test("SUPER qualified in type tree") {
    assert((new SuperTypes.Baz().baz: "Foo.foo") === "Foo.foo")
  }

  test("SUPERtype qualified in type") {
    val baz = new SuperTypes.Baz()
    assert(("" match { case baz.A(x) => x: "Foo.foo" }) === "Foo.foo")
  }
}
