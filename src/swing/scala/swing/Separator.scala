/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing._

/**
 * A bar that can be used a separator, most commonly in menus.
 *
 * @see javax.swing.JSeparator
 */
class Separator(o: Orientation.Value) extends Component with Oriented.Wrapper {
  override lazy val peer: JSeparator = new JSeparator(o.id) with SuperMixin
  def this() = this(Orientation.Horizontal)
}
