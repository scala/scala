package tastytest

import InlineCompat2._

object TestInlineCompat2 extends Suite("TestInlineCompat2") {
  test(assert(foo("Hello, World!") == "Hello, World!"))
}
