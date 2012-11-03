/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing.{JSlider, JLabel}
import event._

/**
 * Lets users select a value from a given range. Visually, this is represented
 * as a draggable knob on a horizontal or vertical bar.
 *
 * Fires a ValueChanged event whenever the slider's value changes and
 * when the knob is released.
 *
 * @see javax.swing.JSlider
 */
class Slider extends Component with Orientable.Wrapper with Publisher {
  override lazy val peer: JSlider = new JSlider with SuperMixin

  def min: Int = peer.getMinimum
  def min_=(v: Int) { peer.setMinimum(v) }
  def max: Int = peer.getMaximum
  def max_=(v: Int) { peer.setMaximum(v) }
  def value: Int = peer.getValue
  def value_=(v: Int) { peer.setValue(v) }
  def extent: Int = peer.getExtent
  def extent_=(v: Int) { peer.setExtent(v) }

  def paintLabels: Boolean = peer.getPaintLabels
  def paintLabels_=(v: Boolean) { peer.setPaintLabels(v) }
  def paintTicks: Boolean = peer.getPaintTicks
  def paintTicks_=(v: Boolean) { peer.setPaintTicks(v) }
  def paintTrack: Boolean = peer.getPaintTrack
  def paintTrack_=(v: Boolean) { peer.setPaintTrack(v) }

  def snapToTicks: Boolean = peer.getSnapToTicks
  def snapToTicks_=(v: Boolean) { peer.setSnapToTicks(v) }

  def minorTickSpacing: Int = peer.getMinorTickSpacing
  def minorTickSpacing_=(v: Int) { peer.setMinorTickSpacing(v) }
  def majorTickSpacing: Int = peer.getMajorTickSpacing
  def majorTickSpacing_=(v: Int) { peer.setMajorTickSpacing(v) }

  def adjusting = peer.getValueIsAdjusting

  def labels: scala.collection.Map[Int, Label] = {
    val labelTable = peer.getLabelTable.asInstanceOf[java.util.Hashtable[Int, JLabel]]
    new scala.collection.JavaConversions.JMapWrapper(labelTable)
      .mapValues(v => UIElement.cachedWrapper[Label](v))
  }
  def labels_=(l: scala.collection.Map[Int, Label]) {
    // TODO: do some lazy wrapping
    val table = new java.util.Hashtable[Any, Any]
    for ((k,v) <- l) table.put(k, v.peer)
    peer.setLabelTable(table)
  }

  peer.addChangeListener(new javax.swing.event.ChangeListener {
    def stateChanged(e: javax.swing.event.ChangeEvent) {
      publish(new ValueChanged(Slider.this))
    }
  })
}
