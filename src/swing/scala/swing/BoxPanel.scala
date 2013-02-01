/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

/**
 * A panel that lays out its contents one after the other,
 * either horizontally or vertically.
 *
 * @see javax.swing.BoxLayout
 */
class BoxPanel(orientation: Orientation.Value) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = {
    val p = new javax.swing.JPanel with SuperMixin
    val l = new javax.swing.BoxLayout(p, orientation.id)
    p.setLayout(l)
    p
  }
}
