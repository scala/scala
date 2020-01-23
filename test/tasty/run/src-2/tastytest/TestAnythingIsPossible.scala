package tastytest

object TestAnythingIsPossible extends Suite("TestAnythingIsPossible") {

  test(assert(new AnythingIsPossible.Class().a === Map("" -> 3)))
  test(assert(new AnythingIsPossible.Lambda().a(3)("abc") === true))
  test(assert(new AnythingIsPossible.Match().a === "zero"))
  test(assert(new AnythingIsPossible.While().a === ()))
  test(assert(new AnythingIsPossible.Assign().a === ()))
  // test(assert(new AnythingIsPossible.Try().a === "nothing")) // bad type on operand stack
  // test(assert(new AnythingIsPossible.Within.Super().a === 23.451)) // bad type on operand stack

}
