package scala.swing

import scala.collection.mutable.Buffer

abstract class IndexedPanel extends Panel {
  override val content: Buffer[Component] = new Content
  def content_=(c: Component*)  { content.clear(); content ++= c }
}
