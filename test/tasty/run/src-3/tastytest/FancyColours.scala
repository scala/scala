package tastytest

object FancyColours:

  trait Pretty:
    self: Colour =>

  trait Dull:
    self: Colour =>

  enum Colour:
    case Pink extends Colour with Pretty
    case Red  extends Colour with Dull

  object Colour:
    def describeRed(r: Red.type) = r.toString
    def describePink(p: Pink.type) = p.toString
