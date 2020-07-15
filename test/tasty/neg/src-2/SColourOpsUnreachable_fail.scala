package tastytest

object SColourOpsUnreachable {

  implicit final class Impl(val c: SColour) extends AnyVal {
    def blue: Int = c match {
      case SColour.Blue => 255
      case SColour.Blue => -1 // error: unreachable
      case _            => -2
    }
  }

}
