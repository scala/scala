package tastytest

import FancyColours._

object TestFancyColours {

  def describe(c: Colour) = c match { // error: would fail on case Red
    case Colour.Pink | Colour.Violet => "Amazing!"
  }

  def describePretty(c: Pretty) = c match { // error: would fail on case Violet
    case Colour.Pink => "Pretty!"
  }

}
