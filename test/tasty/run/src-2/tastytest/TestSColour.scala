package tastytest

object TestSColour extends Suite("TestSColour") {

  implicit final class SColourOps(val c: SColour) extends AnyVal {
    def red: Int = c match {
      case SColour.RGB(r,_,_) => r
      case SColour.Red        => 255
      case _                  => 0
    }
  }

  test("Red.red == 255")(assert(SColour.Red.red === 255))
  test("Green.red == 0")(assert(SColour.Green.red === 0))
  test("RGB(35,61,2).red == 35")(assert(SColour.RGB(35,61,2).red === 35))

}
