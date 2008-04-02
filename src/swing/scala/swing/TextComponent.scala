package scala.swing

import javax.swing._
import javax.swing.text._
import javax.swing.event._
import event._

class TextComponent(override val peer: JTextComponent) extends Component with EditorComponent with Publisher {
  def text: String = peer.getText
  def text_=(x: String) = peer.setText(x)

  val caret = new Caret(peer.getCaret)

  def content: String = peer.getText
  def content_=(v: String) { peer.setText(v) }

  peer.addCaretListener {
    new CaretListener {
      def caretUpdate(e: CaretEvent) { publish(CaretUpdate(TextComponent.this)) }
    }
  }

  lazy val contentModified = liveContentModified

  protected def liveContentModified = new Publisher {
    peer.getDocument.addDocumentListener {
      new DocumentListener {
        override def changedUpdate(e:DocumentEvent) { publish(ContentModified(TextComponent.this)) }
        override def insertUpdate(e:DocumentEvent) { publish(ContentModified(TextComponent.this)) }
        override def removeUpdate(e:DocumentEvent) { publish(ContentModified(TextComponent.this)) }
      }
    }
  }
}

