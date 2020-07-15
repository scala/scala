package tastytest

object FancyColours {
  sealed trait Pretty { self: Colour => }
  sealed trait Dull { self: Colour => }
  enum Colour  {
    case Pink extends Colour with Pretty
    case Violet extends Colour with Pretty
    case Red extends Colour with Dull
  }
}
