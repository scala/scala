package tastytest

/** Suspended as looking for a non-existing ctor with sig (1)java.lang.Enum causes a compiler crash
 */
object TestJColour extends Suite("TestJColour") {

  test(assert(JColour.Red == JColour.Red))
  test(assert(JColour.Green != JColour.Blue))
  test(assert(JColour.Blue.compareTo(JColour.Red) > 0))

}
