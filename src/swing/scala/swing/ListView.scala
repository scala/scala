package scala.swing

import javax.swing._
import javax.swing.event._
import event._

object ListView {
  object IntervalMode extends Enumeration {
    val Single = Value(ListSelectionModel.SINGLE_SELECTION)
    val SingleInterval = Value(ListSelectionModel.SINGLE_INTERVAL_SELECTION)
    val MultiInterval = Value(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
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

  def this(elems: Seq[A]) = {
    this()
    peer.setModel(new AbstractListModel {
      def getElementAt(n: Int) = elems(n).asInstanceOf[AnyRef]
      def getSize = elems.size
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

    object elements extends SeqProxy[A] {
      def self = peer.getSelectedValues.projection.map(_.asInstanceOf[A])
      def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex
    }

    def intervalMode: IntervalMode.Value = IntervalMode(peer.getSelectionModel.getSelectionMode)
    def intervalMode_=(m: IntervalMode.Value) { peer.getSelectionModel.setSelectionMode(m.id) }

    peer.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent) {
        publish(ElementSelected(ListView.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
  }

  def fixedCellWidth = peer.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = peer.setFixedCellWidth(x)

  def fixedCellHeight = peer.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = peer.setFixedCellHeight(x)

  peer.getModel.addListDataListener(new ListDataListener {
    def contentsChanged(e: ListDataEvent) { publish(ListChanged(ListView.this)) }
    def intervalRemoved(e: ListDataEvent) { publish(ListElementsRemoved(ListView.this, e.getIndex0 to e.getIndex1)) }
    def intervalAdded(e: ListDataEvent) { publish(ListElementsAdded(ListView.this, e.getIndex0 to e.getIndex1)) }
  })
}