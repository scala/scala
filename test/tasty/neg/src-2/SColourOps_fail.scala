package tastytest

object SColourOps {

  implicit final class Impl(val c: SColour) extends AnyVal {
    def red: Int = c match { // error: non-exhaustive
      case SColour.Red => 255
    }
    def blue: Int = c match { // error: non-exhaustive
      case SColour.Blue => 255
    }
  }

}
