/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing

import javax.swing.{JScrollPane, ScrollPaneConstants}

object ScrollPane {
  object BarPolicy extends Enumeration {
    import ScrollPaneConstants._
    val AsNeeded = new Value(HORIZONTAL_SCROLLBAR_AS_NEEDED,
                             VERTICAL_SCROLLBAR_AS_NEEDED)
    val Never = new Value(HORIZONTAL_SCROLLBAR_NEVER,
                          VERTICAL_SCROLLBAR_NEVER)
    val Always = new Value(HORIZONTAL_SCROLLBAR_ALWAYS,
                           VERTICAL_SCROLLBAR_ALWAYS)

    def wrap(id: Int) = id match {
      case HORIZONTAL_SCROLLBAR_AS_NEEDED | VERTICAL_SCROLLBAR_AS_NEEDED => AsNeeded
      case HORIZONTAL_SCROLLBAR_NEVER | VERTICAL_SCROLLBAR_NEVER => Never
      case HORIZONTAL_SCROLLBAR_ALWAYS | VERTICAL_SCROLLBAR_ALWAYS => Always
    }
    class Value(val horizontalPeer: Int, val verticalPeer: Int) extends super.Val {
      override def id = horizontalPeer
    }
  }
}

/**
 * Can have at most a single child component, which will be put inside a canvas (the viewport)
 * that can be scrolled.
 *
 * @see javax.swing.JScrollPane
 */
class ScrollPane extends Component with Container {
  import ScrollPane._

  override lazy val peer: JScrollPane = new JScrollPane
  def this(c: Component) = {
    this()
    contents = c
  }
  def contents: Seq[Component] =
    List(UIElement.cachedWrapper(peer.getViewport.getView.asInstanceOf[javax.swing.JComponent]))

  /**
   * Sets the single child.
   */
  def contents_=(c: Component) { peer.setViewportView(c.peer) }

  /**
   * The component being displayed in this pane's row header.
   *
   * If you want to create a row header for lists or tables, you probably
   * want to let the row header be a list view with the same row height as
   * the viewport component.
   */
  def rowHeaderView: Option[Component] = Swing.toOption(peer.getRowHeader.getView).map(UIElement.cachedWrapper(_))
  def rowHeaderView_=(c: Component) = peer.setRowHeaderView(c.peer)
  def rowHeaderView_=(c: Option[Component]) = peer.setRowHeaderView(Swing.toNull(c.map(_.peer)))

  def columnHeaderView: Option[Component] = Swing.toOption(peer.getColumnHeader.getView).map(UIElement.cachedWrapper(_))
  def columnHeaderView_=(c: Component) = peer.setColumnHeaderView(c.peer)
  def columnHeaderView_=(c: Option[Component]) = peer.setColumnHeaderView(Swing.toNull(c.map(_.peer)))

  def viewportView: Option[Component] = Swing.toOption(peer.getViewport.getView).map(UIElement.cachedWrapper(_))
  def viewportView_=(c: Component) = peer.setViewportView(c.peer)
  def viewportView_=(c: Option[Component]) = peer.setViewportView(Swing.toNull(c.map(_.peer)))

  def verticalScrollBarPolicy = BarPolicy.wrap(peer.getVerticalScrollBarPolicy)
  def verticalScrollBarPolicy_=(p: BarPolicy.Value) = peer.setVerticalScrollBarPolicy(p.verticalPeer)

  def horizontalScrollBarPolicy = BarPolicy.wrap(peer.getHorizontalScrollBarPolicy)
  def horizontalScrollBarPolicy_=(p: BarPolicy.Value) = peer.setHorizontalScrollBarPolicy(p.horizontalPeer)

}
