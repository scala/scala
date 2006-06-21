package scala.actors.gui

import javax.swing._
import javax.swing.text.JTextComponent
import javax.swing.event.{CaretEvent,CaretListener}
import event.CaretUpdate

class TextComponent(val jtextcomponent: JTextComponent)
extends Container(jtextcomponent) with SwingComponent with Publisher {

  def text: String = jtextcomponent.getText()
  def text_=(x: String) = jtextcomponent.setText(x)

  val caret = new Caret(jtextcomponent.getCaret())

  jtextcomponent.addCaretListener {
    new CaretListener {
      def caretUpdate(e: CaretEvent) =
        publish(CaretUpdate(TextComponent.this))
    }
  }
}
