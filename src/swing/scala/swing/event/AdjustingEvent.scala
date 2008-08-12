package scala.swing.event

/**
 * An event that indicates some editing operation that can be still in progress.
 * Example: dragging a slider creates a number of AdjustmentEvents with
 * adjusting == true until the user finally releases the mouse button.
 */
trait AdjustingEvent extends ComponentEvent {
  def adjusting: Boolean
  def committed: Boolean = !adjusting
}