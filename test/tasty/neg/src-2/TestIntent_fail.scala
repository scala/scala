package tastytest

import intent._

class TestIntent extends TestSuite with Stateless {
  extension_apply("3 + 3") {
    extension_in("should be 6")(extension_toEqual(expect(3 + 3))(6))
  }
}
