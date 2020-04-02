package tastytest

import FancyColours._

object TestFancyColours extends Suite("TestFancyColours") {

  def describe(c: Colour) = c match {
    case Colour.Pink => "Amazing!"
    case Colour.Red => "Yawn..."
  }

  def describePretty(c: Pretty) = c match {
    case Colour.Pink => "Amazing!"
  }

  def describeDull(c: Dull) = c match {
    case Colour.Red => "Yawn..."
  }

  test(assert(describe(Colour.Pink) === "Amazing!"))
  test(assert(describe(Colour.Red) === "Yawn..."))
  test(assert(describePretty(Colour.Pink) === "Amazing!"))
  test(assert(describeDull(Colour.Red) === "Yawn..."))

}
