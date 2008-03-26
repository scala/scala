package swing

import javax.swing.SwingConstants._

object Orientation {
  def wrap(n: Int): Orientation = n match {
    case HORIZONTAL => Horizontal
    case VERTICAL => Vertical
  }
}
sealed trait Orientation { def peer: Int }
case object Horizontal extends Orientation { def peer = HORIZONTAL }
case object Vertical extends Orientation { def peer = VERTICAL }

object XAlignment {
  def wrap(n: Int): XAlignment = n match {
    case LEFT => Left
    case RIGHT => Right
    case CENTER => Center
  }
}
sealed trait XAlignment { def peer: Int }

object YAlignment {
  def wrap(n: Int): YAlignment = n match {
    case TOP => Top
    case BOTTOM => Bottom
    case CENTER => Center
    //case BASELINE => Baseline
  }
}
sealed trait YAlignment { def peer: Int }
case object Left extends XAlignment { def peer = LEFT }
case object Right extends XAlignment { def peer = RIGHT }
case object Top extends YAlignment { def peer = TOP }
//case object Baseline extends YAlignment { def peer = BASELINE }
case object Bottom extends YAlignment { def peer = BOTTOM }
case object Center extends XAlignment with YAlignment { def peer = CENTER }

/*object Orientation extends Enumeration {
  val Left = Value(LEFT, "left")
  val Right = Value(RIGHT, "right")
  val Bottom = Value(BOTTOM, "bottom")
  val Top = Value(TOP, "top")
  val Center = Value(CENTER, "center")
}*/
