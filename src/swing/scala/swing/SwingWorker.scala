package scala.swing

import scala.actors._

object SwingWorker {

}

abstract class SwingWorker extends Actor {
  def queue() {

  }

  def done() {

  }

  private var _cancelled = false
  def cancelled: Boolean = _cancelled
  def cancelled_=(b: Boolean) { _cancelled = b }
}