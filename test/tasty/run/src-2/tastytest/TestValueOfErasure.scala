package tastytest

object TestValueOfErasure extends Suite("TestValueOfErasure") {

  test(assert(ValueOfErasure.reify[23] === 23))

}
