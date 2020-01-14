package tastytest

object TestSColour extends Suite("TestSColour") {

  implicit final class SColourOps(val c: SColour) extends AnyVal {
    def red: Int = c match {
      case SColour.Red           => 255
      case SColour.Green         => 0
      case SColour.Blue          => 0
      case SColour.RGB(r,_,_)    => r
      case SColour.CMYK(_,y,m,k) => y*m*k
    }
  }

  test("Red.red == 255")(assert(SColour.Red.red === 255))
  test("Green.red == 0")(assert(SColour.Green.red === 0))
  test("Blue.red == 0")(assert(SColour.Blue.red === 0))
  test("RGB(35,61,2).red == 35")(assert(SColour.RGB(35,61,2).red === 35))
  test("CMYK(0,3,4,5).red == 60")(assert(SColour.CMYK(0,3,4,5).red === 60))
  test("redIsRed(Red) == true")(assert(SColour.redIsRed(SColour.Red) === true))

}
