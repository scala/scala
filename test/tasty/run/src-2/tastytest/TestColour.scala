package tastytest

object TestColour extends Suite("TestColour") {

  implicit final class Ops(val c: Colour) extends AnyVal {
    def red: Int = c match {
      // case Colour.Red        => 255 // java.lang.NoSuchMethodError: tastytest.Colour$.Red()Ltastytest/Colour;
      case Colour.RGB(r,_,_) => r
      case _                 => 0
    }
  }

  // test("Red.red == 255")(assert(Colour.Red.red === 255)) // java.lang.NoSuchMethodError: tastytest.Colour$.Red()Ltastytest/Colour;
  test("RGB(10,25,30).red == 10")(assert(Colour.RGB(10,25,30).red === 10))

}
