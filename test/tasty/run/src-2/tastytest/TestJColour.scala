package tastytest

object TestJColour extends Suite("TestJColour") {

  test(assert(JColour.Red == JColour.Red))
  test(assert(JColour.Green != JColour.Blue))
  test(assert(JColour.Blue.compareTo(JColour.Red) > 0))

}
