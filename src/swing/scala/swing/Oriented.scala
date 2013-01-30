/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

object Oriented {
  trait Wrapper extends Oriented {
    def peer: OrientedMixin

    /*
     * Need to revert to structural type, since scroll bars are oriented
     * and these are created by scroll panes. Shouldn't be a bootleneck.
     */
    protected type OrientedMixin = {
      def getOrientation(): Int
      def setOrientation(n: Int)
    }
    def orientation: Orientation.Value = Orientation(peer.getOrientation)
  }
}

/**
 * Something that can have an orientation.
 */
trait Oriented {
  def orientation: Orientation.Value
}
