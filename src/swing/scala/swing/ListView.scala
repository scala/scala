package scala.swing

import javax.swing._
import javax.swing.event._
import event._
import java.awt.Color

object ListView {
  object IntervalMode extends Enumeration {
    val Single = Value(ListSelectionModel.SINGLE_SELECTION)
    val SingleInterval = Value(ListSelectionModel.SINGLE_INTERVAL_SELECTION)
    val MultiInterval = Value(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  }

  def wrap[A](c: JList) = new ListView[A] {
    override lazy val peer = c
  }

  object Renderer {
    def wrap[A](r: ListCellRenderer): Renderer[A] = new Wrapped[A](r)

  	class Wrapped[A](override val peer: ListCellRenderer) extends Renderer[A] {
  	  def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int) = {
        Component.wrap(peer.getListCellRendererComponent(list.peer, a, index, isSelected, hasFocus).asInstanceOf[JComponent])
      }
  	}

    def apply[A,B](f: A => B)(implicit renderer: Renderer[B]): Renderer[A] = new Renderer[A] {
      def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int): Component =
        renderer.componentFor(list, isSelected, hasFocus, f(a), index)
    }
  }

  /*def Renderer[A,B](f: A => B)(implicit renderer: Renderer[B]): Renderer[A] = new Renderer[A] {
    def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int): Component =
      renderer.componentFor(list, isSelected, hasFocus, f(a), index)
  }*/

  abstract class Renderer[-A] {
    def peer: ListCellRenderer = new ListCellRenderer {
      def getListCellRendererComponent(list: JList, a: Any, index: Int, isSelected: Boolean, hasFocus: Boolean) =
        componentFor(ListView.wrap[A](list), isSelected, hasFocus, a.asInstanceOf[A], index).peer
    }
    def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int): Component
  }

  /**
   * A default renderer that maintains a single component for item rendering
   * and preconfigures it to sensible defaults. It is polymorphic on the
   * components type so clients can easily use component specific attributes
   * during configuration.
   */
  abstract class AbstractRenderer[-A, C<:Component](protected val component: C) extends Renderer[A] {
    // The renderer component is responsible for painting selection
    // backgrounds. Hence, make sure it is opaque to let it draw
    // the background.
    component.opaque = true

    /**
     * Standard preconfiguration that is commonly done for any component.
     */
    def preConfigure(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int) {
      if (isSelected) {
        component.background = list.selectionBackground
        component.foreground = list.selectionForeground
      } else {
        component.background = list.background
        component.foreground = list.foreground
      }
    }
    /**
     * Configuration that is specific to the component and this renderer.
     */
    def configure(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int)

    /**
     * Configures the component before returning it.
     */
    def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: A, index: Int): Component = {
      preConfigure(list, isSelected, hasFocus, a, index)
      configure(list, isSelected, hasFocus, a, index)
      component
    }
  }

  implicit object GenericRenderer extends Renderer[Any] {
    override lazy val peer: ListCellRenderer = new DefaultListCellRenderer
    def componentFor(list: ListView[_], isSelected: Boolean, hasFocus: Boolean, a: Any, index: Int): Component = {
      val c = peer.getListCellRendererComponent(list.peer, a, index, isSelected, hasFocus).asInstanceOf[JComponent]
      val w = Component.wrapperFor[Component](c)
      if (w eq null) Component.wrap(c) else w
    }
  }
}

/**
 * A component that displays its elements in a list. Named
 * <code>ListView</code> to avoid a clash with the frequently used
 * <code>scala.List</code>
 *
 * @see javax.swing.JList
 */
class ListView[A] extends Component {
  import ListView._
  override lazy val peer: JList = new JList

  def this(items: Seq[A]) = {
    this()
    listData = items
  }

  protected class ModelWrapper(val items: Seq[A]) extends AbstractListModel {
    def getElementAt(n: Int) = items(n).asInstanceOf[AnyRef]
    def getSize = items.size
  }

  def listData: Seq[A] = peer.getModel match {
    case model: ModelWrapper => model.items
    case model @ _ => new Seq[A] {
     def length = model.getSize
     def elements = new Iterator[A] {
       var idx = 0
       def next = { idx += 1; apply(idx-1) }
       def hasNext = idx < length
     }
     def apply(n: Int) = model.getElementAt(n).asInstanceOf[A]
    }
  }

  def listData_=(items: Seq[A]) {
    peer.setModel(new AbstractListModel {
      def getElementAt(n: Int) = items(n).asInstanceOf[AnyRef]
      def getSize = items.size
    })
  }

  object selection extends Publisher {
    protected abstract class Indices[A](a: =>Seq[A]) extends scala.collection.mutable.Set[A] {
      def -=(n: A)
      def +=(n: A)
      def contains(n: A) = a.contains(n)
      def size = a.length
      def elements = a.elements
    }

    object indices extends Indices(peer.getSelectedIndices) {
      def -=(n: Int) { peer.removeSelectionInterval(n,n) }
      def +=(n: Int) { peer.addSelectionInterval(n,n) }

      def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex
    }
    def selectIndices(ind: Int*) = peer.setSelectedIndices(ind.toArray)

    object items extends SeqProxy[A] {
      def self = peer.getSelectedValues.map(_.asInstanceOf[A])
      def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex
    }

    def intervalMode: IntervalMode.Value = IntervalMode(peer.getSelectionModel.getSelectionMode)
    def intervalMode_=(m: IntervalMode.Value) { peer.getSelectionModel.setSelectionMode(m.id) }

    peer.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent) {
        publish(ListSelectionChanged(ListView.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
  }

  def renderer: ListView.Renderer[A] = ListView.Renderer.wrap(peer.getCellRenderer)
  def renderer_=(r: ListView.Renderer[A]) { peer.setCellRenderer(r.peer) }

  def fixedCellWidth = peer.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = peer.setFixedCellWidth(x)

  def fixedCellHeight = peer.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = peer.setFixedCellHeight(x)

  def prototypeCellValue: A = peer.getPrototypeCellValue.asInstanceOf[A]
  def prototypeCellValue_=(a: A) { peer.setPrototypeCellValue(a) }

  def selectionForeground: Color = peer.getSelectionForeground
  def selectionForeground_=(c: Color) = peer.setSelectionForeground(c)
  def selectionBackground: Color = peer.getSelectionBackground
  def selectionBackground_=(c: Color) = peer.setSelectionBackground(c)

  peer.getModel.addListDataListener(new ListDataListener {
    def contentsChanged(e: ListDataEvent) { publish(ListChanged(ListView.this)) }
    def intervalRemoved(e: ListDataEvent) { publish(ListElementsRemoved(ListView.this, e.getIndex0 to e.getIndex1)) }
    def intervalAdded(e: ListDataEvent) { publish(ListElementsAdded(ListView.this, e.getIndex0 to e.getIndex1)) }
  })
}