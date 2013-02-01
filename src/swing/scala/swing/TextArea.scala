/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import javax.swing._

/**
 * A text component that allows multiline text input and display.
 *
 * @see javax.swing.JTextArea
 */
class TextArea(text0: String, rows0: Int, columns0: Int) extends TextComponent
    with TextComponent.HasColumns with TextComponent.HasRows {
  override lazy val peer: JTextArea = new JTextArea(text0, rows0, columns0) with SuperMixin
  def this(text: String) = this(text, 0, 0)
  def this(rows: Int, columns: Int) = this("", rows, columns)
  def this() = this("", 0, 0)

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
