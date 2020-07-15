package tastytest

object SColourOps {

  implicit final class Impl(val c: SColour) extends AnyVal {
    def red: Int = c match { // will not warn if annotations are turned off
      case SColour.Red => 255
    }
  }

}
