/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import event._
import scala.collection.mutable.Buffer
import javax.swing.{JTabbedPane, JComponent}


object TabbedPane {
  object Layout extends Enumeration {
    val Wrap = Value(JTabbedPane.WRAP_TAB_LAYOUT)
    val Scroll = Value(JTabbedPane.SCROLL_TAB_LAYOUT)
  }

  class Page protected[TabbedPane](parent0: TabbedPane, title0: String, content0: Component, tip0: String) extends Proxy {
    def self = content0

    def this(title0: String, content0: Component, tip0: String) =
      this(null, title0, content0, tip0)
    def this(title0: String, content0: Component) =
      this(title0, content0, "")
    content = content0 // first add component, *then* set other things
    title = title0
    tip = tip0

    protected[TabbedPane] var parent: TabbedPane = parent0

    protected var _title = title0
    def title: String = _title
    def title_=(t: String) {
      // beware to keep this order since, index depends on the _old_ title
      if (parent != null) parent.peer.setTitleAt(index, t)
      _title = t
    }
    protected var _content = content0
    def content: Component = _content//UIElement.cachedWrapper(peer.getComponentAt(index).asInstanceOf[JComponent])
    def content_=(c: Component) { _content = c; if (parent != null) parent.peer.setComponentAt(index, c.peer) }
    protected var _tip = tip0
    def tip: String = _tip//peer.getToolTipTextAt(index)
    def tip_=(t: String) { _tip = t; if (parent != null) parent.peer.setToolTipTextAt(index, t) }
    protected var _enabled = true
    def enabled: Boolean = _enabled//peer.isEnabledAt(index)
    def enabled_=(b: Boolean) { _enabled = b; if (parent != null) parent.peer.setEnabledAt(index, b) }
    protected var _mnemonic = -1
    def mnemonic: Int = _mnemonic//peer.getMnemonicAt(index)
    def mnemonic_=(k: Int) { _mnemonic = k; if (parent != null) parent.peer.setMnemonicAt(index, k)}
    protected var _foreground: Color = null
    def foreground: Color = _foreground//peer.getForegroundAt(index)
    def foreground_=(c: Color) { _foreground = c; if (parent != null) parent.peer.setForegroundAt(index, c)}
    protected var _background: Color = null
    def background: Color = _background //peer.getBackgroundAt(index)
    def background_=(c: Color) { _background = c; if (parent != null) parent.peer.setBackgroundAt(index, c)}
    def bounds: Rectangle = parent.peer.getBoundsAt(index)

    // TODO: icon, disabledIcon

    def index = if(parent != null) parent.peer.indexOfTab(title) else 0//_index
    //protected[TabbedPane] var _index: Int = index0
  }
}

/**
 * Displays the contents of one of several pages at a time. For each page a tab is
 * visible at all times. The user can click on one of these tabs to move the
 * corresponding page to the front.
 *
 * @see javax.swing.JTabbedPane
 */
class TabbedPane extends Component with Publisher {
  override lazy val peer: JTabbedPane = new JTabbedPane with SuperMixin
  import TabbedPane._

  object pages extends BufferWrapper[Page] {
    def runCount: Int = peer.getTabRunCount

    def remove(n: Int): Page = {
      val t = apply(n)
      peer.removeTabAt(n)
      t.parent = null
      //for(i <- n to length) apply(i)._index -= 1
      t
    }
    protected def insertAt(n: Int, t: Page) {
      //for(i <- n to length) apply(i)._index += 1
      t.parent = TabbedPane.this
      peer.insertTab(t.title, null, t.content.peer, t.tip, n)
    }

    def +=(t: Page): this.type = { t.parent = TabbedPane.this; peer.addTab(t.title, null, t.content.peer, t.tip); this }
    def length = peer.getTabCount
    def apply(n: Int) = new Page(TabbedPane.this, peer.getTitleAt(n),
      UIElement.cachedWrapper[Component](peer.getComponentAt(n).asInstanceOf[javax.swing.JComponent]),
      peer.getToolTipTextAt(n))
  }

  def tabLayoutPolicy: Layout.Value = Layout(peer.getTabLayoutPolicy)
  def tabLayoutPolicy_=(p: Layout.Value) { peer.setTabLayoutPolicy(p.id) }


  def tabPlacement: Alignment.Value = Alignment(peer.getTabPlacement)
  /**
   * Possible values are Left, Right, Top, Bottom.
   */
  def tabPlacement_=(b: Alignment.Value) { peer.setTabPlacement(b.id) }

  /**
   * The current page selection
   */
  object selection extends Publisher {
    def page: Page = pages(index)
    def page_=(p: Page) { index = p.index }

    def index: Int = peer.getSelectedIndex
    def index_=(n: Int) { peer.setSelectedIndex(n) }

    peer.addChangeListener(new javax.swing.event.ChangeListener {
      def stateChanged(e: javax.swing.event.ChangeEvent) {
        publish(SelectionChanged(TabbedPane.this))
      }
    })
  }
}
