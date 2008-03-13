package swing;

import javax.swing._;
import event._;

class Frame(val jframe: JFrame) extends Container(jframe) with Publisher {
  def this() = this(new JFrame("Untitled Frame"))
  def title: String = jframe.getTitle()
  def title_=(s: String) = jframe.setTitle(s)
  val contents = new Container(jframe.getContentPane())
  private var default_button: Button = null
 	def defaultButton = default_button
  def defaultButton_=(b: Button) = { default_button = b; jframe.getRootPane().setDefaultButton(b.jbutton) }
  def pack: this.type = { jframe.pack(); this }
  jframe.addWindowListener {
    new java.awt.event.WindowListener {
      def windowActivated(e: java.awt.event.WindowEvent) = publish(WindowActivated(Frame.this))
      def windowClosed(e: java.awt.event.WindowEvent) = publish(WindowClosed(Frame.this))
      def windowClosing(e: java.awt.event.WindowEvent) = publish(WindowClosing(Frame.this))
      def windowDeactivated(e: java.awt.event.WindowEvent) = publish(WindowDeactivated(Frame.this))
      def windowDeiconified(e: java.awt.event.WindowEvent) = publish(WindowDeiconified(Frame.this))
      def windowIconified(e: java.awt.event.WindowEvent) = publish(WindowIconified(Frame.this))
      def windowOpened(e: java.awt.event.WindowEvent) = publish(WindowOpened(Frame.this))
    }
  }
}
