package tastytest

object FancyColours {
  trait Pretty { self: Colour => }
  trait Dull   { self: Colour => }
  enum Colour  {
    case Pink extends Colour with Pretty
    case Red  extends Colour with Dull
  }
}
