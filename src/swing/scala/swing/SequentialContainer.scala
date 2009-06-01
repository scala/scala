package scala.swing

import scala.collection.mutable.Buffer

object SequentialContainer {
  /**
   * Utility trait for wrapping sequential containers.
   */
  trait Wrapper extends SequentialContainer with Container.Wrapper {
    override val contents: Buffer[Component] = new Content
    //def contents_=(c: Component*)  { contents.clear(); contents ++= c }
  }
}

/**
 * A container for which a sequential order of children makes sense, such as
 * flow panels, or menus. Its contents are mutable.
 */
trait SequentialContainer extends Container {
  /**
   * The mutable child components of this container. The order matters and
   * usually indicates the layout of the children.
   */
  override def contents: Buffer[Component]
  //def contents_=(c: Component*)
}
