package scala.swing.event

trait InputEvent extends ComponentEvent {
  def peer: java.awt.event.InputEvent
  def when: Long = peer.getWhen
  def modifiers: Int
  def consume() { peer.consume() }
  def consumed: Boolean = peer.isConsumed
}
