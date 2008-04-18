package scala.swing

import scala.collection.mutable.Buffer

trait IndexedContainer extends Container {
  override val contents: Buffer[Component] = new Content
  def contents_=(c: Component*)  { contents.clear(); contents ++= c }
}
