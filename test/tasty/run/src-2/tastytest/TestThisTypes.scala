package tastytest

import ThisTypes._

object TestThisTypes extends Suite("TestThisTypes") {

  test(assert(new Sub().doTest.get.x === 23))
  test(assert(new Sub2().doTest.get.x === 23))

}
