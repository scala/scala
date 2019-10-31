package tastytest

import Extensions._

/**
 * Suspended until extension methods on anyvals are supported
 */
object TestExtensions extends Suite("TestExtensions") {
  test(assert(().hello === "Hello"))
}
