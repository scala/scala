package scala.swing

import javax.swing.JApplet

/**
 * Clients should implement the ui field. See the SimpleApplet demo for an example.
 *
 * Note: Applet extends JApplet to satisfy Java's applet loading mechanism.
 * The usual component wrapping scheme doesn't work here.
 *
 * @see javax.swing.JApplet
 */
abstract class Applet extends JApplet { outer =>
  val ui: UI

  override def init() { ui.init() }
  override def start() { ui.start() }
  override def stop() { ui.stop() }

  abstract class UI extends RootPanel {
    def peer = outer
    override def contents_=(c: Component) {
      super.contents_=(c)
      peer.validate()
    }

    def init()
    def start() {}
    def stop() {}
  }
}

