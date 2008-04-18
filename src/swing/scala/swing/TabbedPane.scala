package scala.swing

import geometry._
import scala.collection.mutable.Buffer
import javax.swing.{JTabbedPane, JComponent}
import java.awt.Color


object TabbedPane {
  object Layout extends Enumeration {
    val Wrap = Value(JTabbedPane.WRAP_TAB_LAYOUT)
    val Scroll = Value(JTabbedPane.SCROLL_TAB_LAYOUT)
  }
}

class TabbedPane(override val peer: JTabbedPane) extends Component with Publisher {
  import TabbedPane._

  class Tab protected(title0: String, component0: Component, tip0: String, index0: Int) {
    def this(title0: String, component0: Component, tip0: String) =
      this(title0, component0, tip0, 0)
    title = title0
    component = component0
    tip = tip0
    def title: String = peer.getTitleAt(index)
    def title_=(t: String) { peer.setTitleAt(index, t) }
    def component: Component = Component.wrapperFor(peer.getComponentAt(index).asInstanceOf[JComponent])
    def component_=(c: Component) { peer.setComponentAt(index, c.peer) }
    def tip: String = peer.getToolTipTextAt(index)
    def tip_=(t: String) { peer.setToolTipTextAt(index, t) }
    def enabled: Boolean = peer.isEnabledAt(index)
    def enabled_=(b: Boolean) { peer.setEnabledAt(index, b) }
    def mnemonic: Int = peer.getMnemonicAt(index)
    def mnemonic_=(k: Int) = peer.setMnemonicAt(index, k)
    def foreground: Color = peer.getForegroundAt(index)
    def foreground_=(c: Color) = peer.setForegroundAt(index, c)
    def background: Color = peer.getBackgroundAt(index)
    def background_=(c: Color) = peer.setBackgroundAt(index, c)
    def bounds: Rectangle = Rectangle.wrap(peer.getBoundsAt(index))

    // TODO: icon, disabledIcon

    def index = _index
    protected[TabbedPane] var _index: Int = index0
  }

  def this() = this(new JTabbedPane)

  object tabs extends BufferAdapter[Tab] {
    def runCount: Int = peer.getTabRunCount

    def remove(n: Int): Tab = {
      val t = apply(n)
      peer.removeTabAt(n)
      for(i <- n to length) apply(i)._index -= 1
      t
    }
    protected def insertAt(n: Int, t: Tab) {
      for(i <- n to length) apply(i)._index += 1
      peer.insertTab(t.title, null, t.component.peer, t.tip, n)
    }

    def +=(t: Tab) { peer.addTab(t.title, null, t.component.peer, t.tip) }
    def length = peer.getTabCount
    def apply(n: Int) = new Tab(peer.getTitleAt(n),
                                Component.wrapperFor(peer.getComponentAt(n).asInstanceOf[javax.swing.JComponent]),
                                peer.getToolTipTextAt(n))
  }

  def tabLayoutPolicy: Layout.Value = Layout(peer.getTabLayoutPolicy)
  def tabLayoutPolicy_=(p: Layout.Value) { peer.setTabLayoutPolicy(p.id) }

  def tabPlacement: Edge = Edge.wrap(peer.getTabPlacement)
  def tabPlacement(b: Edge) { peer.setTabPlacement(b.peer) }

  def selected: Tab = tabs(peer.getSelectedIndex)
}
