package tastytest

import InlineCompat._

object TestInlineCompat extends Suite("TestInlineCompat") {
  test(assert(foo("Hello, World!") == "Hello, World!"))
}
