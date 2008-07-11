package scala.swing.event

object LiveEvent {
  def unapply(e: LiveEvent): Option[(Component, Boolean)] = Some(e.source, e.live)
}

/**
 * An event that indicates some editing operation that can be explicitly
 * finished by some final action. Example: entering text in a text field
 * (not a text area) is finalized not before the user hits enter.
 */
trait LiveEvent extends ComponentEvent {
  def live: Boolean
}
