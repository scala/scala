package scala.swing

import javax.swing.JApplet

/**
 * Clients should implement the ui field. See the SimpleApplet demo for a sample.
 *
 * Note: Applet extends JApplet to satisfy Java's applet loading mechanism.
 * The usual component wrapping scheme doesn't work here.
 */
abstract class Applet extends JApplet { outer =>
  val ui: UI

  override def init() { ui.init() }
  override def start() { ui.start() }
  override def stop() { ui.stop() }

  abstract class UI extends RootPanel {
    def peer = outer
    override def content_=(c: Component) {
      super.content_=(c)
      peer.validate()
    }

    def init()
    def start() {}
    def stop() {}
  }
}

