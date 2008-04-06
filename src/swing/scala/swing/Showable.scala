package scala.swing

object Showable {
  trait Swing extends Showable {
    def peer: { def setVisible(b: Boolean) }
    def show(): this.type = { peer.setVisible(true); this }
  }
}

trait Showable {
  def show(): this.type
}
