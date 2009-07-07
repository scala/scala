/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing

/**
 * Something that can have an orientation.
 */
trait Oriented {
  def peer: javax.swing.JComponent with OrientedMixin

  protected trait OrientedMixin {
    def getOrientation(): Int
    def setOrientation(n: Int)
  }
  def orientation: Orientation.Value = Orientation(peer.getOrientation)
}
