/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import javax.swing._
import javax.swing.text._

/**
 * A text component that allows multiline text input and display.
 *
 * @see javax.swing.JEditorPane
 */
class EditorPane(contentType0: String, text0: String) extends TextComponent {
	override lazy val peer: JEditorPane = new JEditorPane(contentType0, text0) with SuperMixin
	def this() = this("text/plain", "")

	def contentType: String = peer.getContentType
	def contentType_=(t: String) = peer.setContentType(t)

	def editorKit: EditorKit = peer.getEditorKit
	def editorKit_=(k: EditorKit) = peer.setEditorKit(k)
}
