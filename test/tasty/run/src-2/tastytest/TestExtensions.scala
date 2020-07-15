package tastytest

import Extensions._

object TestExtensions extends Suite("TestExtensions") {
  test(assert(().hello === "Hello"))
}
