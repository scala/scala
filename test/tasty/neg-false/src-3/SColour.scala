package tastytest

sealed abstract class SColour
object SColour {
  case object      Red                         extends SColour
  case object      Green                       extends SColour
  case object      Blue                        extends SColour
  final case class RGB(r: Int, g: Int, b: Int) extends SColour
}
