package tastytest

sealed abstract class SColour
object SColour {
  case object      Red                                  extends SColour
  case object      Green                                extends SColour
  case object      Blue                                 extends SColour
  final case class RGB(r: Int, g: Int, b: Int)          extends SColour
  final case class CMYK(c: Int, m: Int, y: Int, k: Int) extends SColour

  def redIsRed(r: Red.type): Boolean = r == Red
}
