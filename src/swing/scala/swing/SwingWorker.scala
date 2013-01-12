package scala.swing

import scala.actors._

@deprecated("Will be removed in 2.11.0", "2.10.1")
object SwingWorker {

}

@deprecated("Depends on the deprecated package scala.actors. Will be removed in 2.11.0", "2.10.1")
abstract class SwingWorker extends Actor {
  def queue() {

  }

  def done() {

  }

  private var _cancelled = false
  def cancelled: Boolean = _cancelled
  def cancelled_=(b: Boolean) { _cancelled = b }
}
