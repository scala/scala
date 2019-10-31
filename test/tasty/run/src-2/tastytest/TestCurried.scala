package tastytest

object TestCurried extends Suite("TestCurried") {

  test(assert(new CurriedCtor(99)("")(false).a === 99))
  test(assert(new CurriedCtor(0)("foo")(false).s === "foo"))
  test(assert(new CurriedCtor(0)("")(true).b === true))
  test(assert(new Overrider(101)(false).a === 101))
  test(assert(new Overrider(0)(false).s === "overrider"))
  test(assert(new Overrider(0)(true).b === true))

}
