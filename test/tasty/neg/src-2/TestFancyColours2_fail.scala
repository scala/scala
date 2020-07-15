package tastytest

import FancyColours._

object TestFancyColours2 {

  def describePretty(c: Pretty) = c match {
    case Colour.Pink   => "Pretty!"
    case Colour.Violet => "Pretty!"
    case Colour.Red    => "Pretty!" // error: Red not instance of Pretty
  }

  def describeDull(c: Dull) = c match {
    case Colour.Pink => "Dull!" // error: Pink not instance of Dull
    case Colour.Red  => "Dull!"
  }

}
