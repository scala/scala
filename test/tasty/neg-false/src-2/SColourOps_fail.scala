package tastytest

/** Suspended as objects inside object are not correctly handled by the pattern exhaustivity checker.
 */
object SColourOps {

  implicit final class Impl(val c: SColour) extends AnyVal {
    def red: Int = c match {
      case SColour.RGB(r,_,_)     => r
      case SColour.CMYK(_,y,m,k)  => y*m*k // error: unreachable code
      case SColour.Red            => 255
      case SColour.Green          => 0
    }
  }

}
