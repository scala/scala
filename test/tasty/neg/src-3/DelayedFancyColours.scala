package tastytest

object DelayedFancyColours {

  sealed trait Pretty { self: FancyColours.Colour => }

  object FancyColours {
    sealed trait Dull { self: Colour => }
    enum Colour  {
      case Pink extends Colour with Pretty
      case Violet extends Colour with Pretty
      case Red extends Colour with Dull
    }
  }

}
