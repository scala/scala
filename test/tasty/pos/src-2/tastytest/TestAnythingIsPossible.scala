package tastytest

object TestAnythingIsPossible extends Suite("TestAnythingIsPossible") {

  test(assert(new AnythingIsPossible.Try().a === "nothing"))
  test(assert(new AnythingIsPossible.Within.Super().a === 23.451))

}
