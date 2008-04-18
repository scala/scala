package scala.swing

import javax.swing._
import javax.swing.text._
import javax.swing.event._
import event._

object TextComponent {
  trait HasColumns extends TextComponent {
    def columns: Int
    def columns_=(n: Int)
  }
  trait HasRows extends TextComponent {
    def rows: Int
    def rows_=(n: Int)
  }
}

class TextComponent(override val peer: JTextComponent) extends Component with EditorComponent with Publisher {
  def contents: String = peer.getText
  def contents_=(t: String) = peer.setText(t)

  val caret = new Caret(peer.getCaret)

  def editable: Boolean = peer.isEditable
  def editable_=(x: Boolean) = peer.setEditable(x)

  def cut() { peer.cut() }
  def copy() { peer.copy() }
  def selected: String = peer.getSelectedText

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

