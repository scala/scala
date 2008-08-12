package scala.swing.event

trait InputEvent extends ComponentEvent {
  val when: Long
  val modifiers: Int
}
