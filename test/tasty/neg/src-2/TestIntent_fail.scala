package tastytest

import intent._

class TestIntent extends TestSuite with Stateless {
  apply("3 + 3") {
    in("should be 6")(toEqual(expect(3 + 3))(6))
  }
}
