package tastytest

object TestCurriedOld extends Suite("TestCurriedOld") {

  test(assert(new CurriedCtorOld(0)("foo")(false).s === "foo"))
  test(assert(new OverriderOld(0)(true).a === true))

}
