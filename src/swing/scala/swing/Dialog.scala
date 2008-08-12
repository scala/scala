package scala.swing

import javax.swing.{Icon, JOptionPane}

object Dialog {
  object Message extends Enumeration {
    val Error = Value(JOptionPane.ERROR_MESSAGE)
    val Info = Value(JOptionPane.INFORMATION_MESSAGE)
    val Warning = Value(JOptionPane.WARNING_MESSAGE)
    val Question = Value(JOptionPane.QUESTION_MESSAGE)
    val Plain = Value(JOptionPane.PLAIN_MESSAGE)
  }
  object Options extends Enumeration {
    val Default = Value(JOptionPane.DEFAULT_OPTION)
    val YesNo = Value(JOptionPane.YES_NO_OPTION)
    val YesNoCancel = Value(JOptionPane.YES_NO_CANCEL_OPTION)
    val OkCancel = Value(JOptionPane.OK_CANCEL_OPTION)
  }
  object Result extends Enumeration {
    val Yes = Value(JOptionPane.YES_OPTION)
    val Ok = Yes
    val No = Value(JOptionPane.NO_OPTION)
    val Cancel = Value(JOptionPane.CANCEL_OPTION)
    val Closed = Value(JOptionPane.CLOSED_OPTION)
  }


  def showConfirmation(parent: Component, message: String, title: String,
     optionType: Options.Value, messageType: Message.Value, icon: Icon): Result.Value =
     Result(JOptionPane.showConfirmDialog(parent.peer, message, title,
                                   optionType.id, messageType.id, Swing.wrapIcon(icon)))
  def showConfirmation(parent: Component, message: String, title: String,
     optionType: Options.Value): Result.Value =
     Result(JOptionPane.showConfirmDialog(parent.peer, message, title,
                                   optionType.id))

  def showOptions(parent: Component, message: String, title: String,
     optionType: Options.Value, messageType: Message.Value, icon: Icon,
     entries: Seq[Any], initialEntry: Int): Result.Value = {
       val r = JOptionPane.showOptionDialog(parent.peer, message, title,
                                   optionType.id, messageType.id, Swing.wrapIcon(icon),
                                   entries.map(_.asInstanceOf[AnyRef]).toArray, entries(initialEntry))
       Result(r)
     }

  def showInput[A](parent: Component, message: String, title: String,
                   messageType: Message.Value, icon: Icon,
     entries: Seq[A], initialEntry: A): Option[A] = {
       val e = if (entries.isEmpty) null
               else entries.map(_.asInstanceOf[AnyRef]).toArray
       val r = JOptionPane.showInputDialog(parent.peer, message, title,
       		                               messageType.id, Swing.wrapIcon(icon),
       		                               e, initialEntry)
       Swing.toOption(r)
  }
  def showMessage(parent: Component, message: String, title: String,
     messageType: Message.Value, icon: Icon) {
     JOptionPane.showMessageDialog(parent.peer, message, title,
                                   messageType.id, Swing.wrapIcon(icon))
  }

  def showMessage(parent: Component, message: String) {
     JOptionPane.showMessageDialog(parent.peer, message)
  }
}
