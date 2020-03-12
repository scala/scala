package tastytest

object TestColour extends Suite("TestColour") {

  implicit final class Ops(val c: Colour) extends AnyVal {
    def red: Int = c match {
      case Colour.Red        => 255
      case Colour.RGB(r,_,_) => r
      case _                 => 0
    }
    def green: Int = c match {
      case Colour.Green      => 255
      case Colour.RGB(_,g,_) => g
      case _                 => 0
    }
    def blue: Int = c match {
      case Colour.Blue       => 255
      case Colour.RGB(_,_,b) => b
      case _                 => 0
    }
    def foo: Int = c match {
      case Colour.Red           => -1
      case Colour.Green         => 0
      case Colour.Blue          => 1
      case Colour.RGB(r,g,b)    => r*g*b
      case Colour.CMYK(c,y,m,k) => c*y*m*(-k)
    }
  }

  def constRed: Colour.Red.type = Colour.Red

  test("Red.red == 255")(assert(Colour.Red.red === 255))
  test("constRed.red == 255")(assert(constRed.red === 255))
  test("RGB(10,25,30).red == 10")(assert(Colour.RGB(10,25,30).red === 10))
  test("Green.green == 255")(assert(Colour.Green.green === 255))
  test("RGB(10,25,30).green == 25")(assert(Colour.RGB(10,25,30).green === 25))
  test("Blue.blue == 255")(assert(Colour.Blue.blue === 255))
  test("RGB(10,25,30).blue == 30")(assert(Colour.RGB(10,25,30).blue === 30))
  test("Red.foo == -1")(assert(Colour.Red.foo === -1))
  test("Green.foo == 0")(assert(Colour.Green.foo === 0))
  test("Blue.foo == 1")(assert(Colour.Blue.foo === 1))

}
