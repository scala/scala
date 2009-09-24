package scala.swing

import java.awt.{Adjustable => JAdjustable}

object Adjustable {
  trait Wrapper extends Oriented.Wrapper with Adjustable {
    def peer: JAdjustable with OrientedMixin

    def unitIncrement = peer.getUnitIncrement
    def unitIncrement_=(i: Int) = peer.setUnitIncrement(i)
    def blockIncrement = peer.getBlockIncrement
    def blockIncrement_=(i: Int) = peer.setBlockIncrement(i)

    def value = peer.getValue
    def value_=(v: Int) = peer.setValue(v)

    def visibleAmount = peer.getVisibleAmount
    def visibleAmount_=(v: Int) = peer.setVisibleAmount(v)

    def minimum = peer.getMinimum
    def minimum_=(m: Int) = peer.setMinimum(m)
    def maximum = peer.getMaximum
    def maximum_=(m: Int) = peer.setMaximum(m)
  }
}

trait Adjustable extends Oriented {
  def unitIncrement: Int
  def unitIncrement_=(i: Int)
  def blockIncrement: Int
  def blockIncrement_=(i: Int)

  def value: Int
  def value_=(v : Int)

  def visibleAmount: Int
  def visibleAmount_=(v: Int)

  def minimum: Int
  def minimum_=(m: Int)
  def maximum: Int
  def maximum_=(m: Int)

// Needs implementation of AdjustmentEvent
//
//    val adjustments: Publisher = new Publisher {
//		peer.addAdjustmentListener(new AdjustmentListener {
//			def adjustmentValueChanged(e: java.awt.event.AdjustmentEvent) {
//				publish(new AdjustmentEvent(e))
//			}
//		})
//   	}
}
