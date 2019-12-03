package tastytest

/** Suspended as enums are not correctly handled by the pattern exhaustivity checker.
 */
object ColourOps {

  implicit final class Impl(val c: Colour) extends AnyVal {
    def r: Int = c match {
      case Colour.Red        => 255
      case Colour.RGB(r,_,_) => r   // error: unreachable code
      case _                 => 0
    }
  }

}
