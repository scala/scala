package tastytest

import Enums._

object TestEnum extends Suite("TestEnum") {

  final class Colour(name: String, ordinal: Int) extends Enum[Colour](name, ordinal)

  object Colour extends EnumCompanion[Colour](implicitly)() {
    val Red   = new Colour("Red", 0)
    val Green = new Colour("Green", 1)
    val Blue  = new Colour("Blue", 2)
  }

  test(assert(Colour.Red == Colour.Red))
  test(assert(Colour.Green != Colour.Blue))
  test(assert(Colour.Blue > Colour.Red))
  test(assert(Colour.valueOf("Green") == Colour.Green))
  test(assert(Colour.values `sameElements` Array(Colour.Red, Colour.Green, Colour.Blue)))

}
