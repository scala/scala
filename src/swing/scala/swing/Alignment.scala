/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing.SwingConstants._

/**
 * Horizontal and vertical alignments. We sacrifice a bit of type-safety
 * for simplicity here.
 *
 * @see javax.swing.SwingConstants
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

