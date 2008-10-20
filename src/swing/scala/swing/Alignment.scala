package scala.swing

import javax.swing.SwingConstants._

/**
 * Horizontal and vertical alignments. We sacrifice a bit of type-safety
 * for simplicity here.
 */
object Alignment extends Enumeration {
  val Left = Value(LEFT)
  val Right = Value(RIGHT)
  val Center = Value(CENTER)
  val Top = Value(TOP)
  val Bottom = Value(BOTTOM)
  //1.6: val Baseline = Value(BASELINE)

  val Leading = Value(LEADING)
  val Trailing = Value(TRAILING)
}

