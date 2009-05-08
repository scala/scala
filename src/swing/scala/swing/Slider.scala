package scala.swing

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
class Slider extends Component with Orientable with Publisher {
  override lazy val peer: javax.swing.JSlider = new javax.swing.JSlider

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

  def labels: scala.collection.Map[Int, Label] =
    new scala.collection.JavaConversions.JMapWrapper[Int, Label](
      peer.getLabelTable.asInstanceOf[java.util.Hashtable[Int, Label]])
  def labels_=(l: scala.collection.Map[Int, Label]) {
    val table = new java.util.Hashtable[Any, Any]
    for ((k,v) <- l) table.put(k, v)
    peer.setLabelTable(table)
  }

  peer.addChangeListener(new javax.swing.event.ChangeListener {
    def stateChanged(e: javax.swing.event.ChangeEvent) {
      publish(ValueChanged(Slider.this))
    }
  })
}
