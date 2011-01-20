/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

object Orientable {
  trait Wrapper extends Oriented.Wrapper with Orientable {
    def orientation_=(o: Orientation.Value) { peer.setOrientation(o.id) }
  }
}

/**
 * An <code>Oriented</code> whose orientation can be changed.
 */
trait Orientable extends Oriented {
  def orientation_=(o: Orientation.Value)
}
