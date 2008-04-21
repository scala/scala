package scala.swing

import javax.swing.{AbstractButton => JAbstractButton,Icon}
import event._

/**
 * @see javax.swing.AbstractButton
 */
abstract class Button(override val peer: JAbstractButton) extends Component(peer) with Action.Trigger with Publisher {
  def text: String = peer.getText
  def text_=(s: String) = peer.setText(s)
  def icon: Icon = peer.getIcon
  def icon_=(i: Icon) = peer.setIcon(i)

  private var _action: Action = Action.NoAction
  def action: Action = _action
  def action_=(a: Action) { _action = a; peer.setAction(a.peer) }

  //1.6: def hideActionText: Boolean = peer.getHideActionText
  //def hideActionText_=(b: Boolean) = peer.setHideActionText(b)

  peer.addActionListener(new java.awt.event.ActionListener {
    def actionPerformed(e: java.awt.event.ActionEvent) {
      publish(ButtonClicked(Button.this))
    }
  })

  def selected: Boolean = peer.isSelected
  def selected_=(b: Boolean) = peer.setSelected(b)
}
