package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JTextArea
 */
class TextArea(override val peer: JTextArea) extends TextComponent(peer) with TextComponent.HasColumns with TextComponent.HasRows {
  def this(text: String, rows: Int, columns: int) = this(new JTextArea(text, rows, columns))
  def this(text: String) = this(new JTextArea(text))
  def this(rows: Int, columns: int) = this(new JTextArea(rows, columns))
  def this() = this(new JTextArea())

  // TODO: we could make contents StringBuilder-like
  def append(t: String) { peer.append(t) }

  def rows: Int = peer.getRows
  def rows_=(n: Int) = peer.setRows(n)
  def columns: Int = peer.getColumns
  def columns_=(n: Int) = peer.setColumns(n)

  def tabSize: Int = peer.getTabSize
  def tabSize_=(n: Int) = peer.setTabSize(n)
  def lineCount: Int = peer.getLineCount

  def lineWrap: Boolean = peer.getLineWrap
  def lineWrap_=(w: Boolean) = peer.setLineWrap(w)
  def wordWrap: Boolean = peer.getWrapStyleWord
  def wordWrap_=(w: Boolean) = peer.setWrapStyleWord(w)
  def charWrap: Boolean = !peer.getWrapStyleWord
  def charWrap_=(w: Boolean) = peer.setWrapStyleWord(!w)
}