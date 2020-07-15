package tastytest

import FancyColours._

object TestFancyColours3 {

  def describeSingleton(c: scala.deriving.Mirror.Singleton) = c match {
    case Colour.Red    => "Colour.Red"
    case Colour.Pink   => "Colour.Pink"
    case Colour.Violet => "Colour.Violet"
  }

  def test = describeSingleton(Colour.Pink)

}
