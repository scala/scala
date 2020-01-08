package tastytest

object SColourOps {

  implicit final class Impl(val c: SColour) extends AnyVal {
    def red: Int = c match {
      case SColour.Red => 255 // should error because it is not exhaustive
    }
  }

}
