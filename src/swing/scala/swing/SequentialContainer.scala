package scala.swing

import scala.collection.mutable.Buffer

object SequentialContainer {
  /**
   * Utility trait for wrapping sequential containers.
   */
  trait Wrapper extends Component with SequentialContainer with Container.Wrapper {
    override val contents: Buffer[Component] = new Content
    //def contents_=(c: Component*)  { contents.clear(); contents ++= c }
  }
}

/**
 * A container that contains children in a specific sequential order.
 */
trait SequentialContainer extends Container {
  override def contents: Buffer[Component]
  //def contents_=(c: Component*)
}
