/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

/**
 * A component that can contain other components.
 *
 * @see javax.swing.JPanel
 */
abstract class Panel extends Component with Container.Wrapper {
  override lazy val peer: javax.swing.JPanel = new javax.swing.JPanel with SuperMixin
}
