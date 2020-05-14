package scala.collection

import org.junit._
import scala.collection.mutable.ListBuffer
import org.openjdk.jol.info.GraphLayout

class Sizes {
  import JOL._
  @Test
  def list: Unit = {
    assertTotalSize(24, JOL.netLayout(null :: Nil, Nil))
  }
  @Test
  def listBuffer: Unit = {
    assertTotalSize(32, JOL.netLayout(new ListBuffer[String], Nil))
  }
  @Test
  def wrappedArray: Unit = {
    val wrapped = Array[String]("")
    assertTotalSize(16, JOL.netLayout(mutable.ArraySeq.make[String](wrapped), wrapped))
    assertTotalSize(16, JOL.netLayout(immutable.ArraySeq.unsafeWrapArray[String](wrapped), wrapped))
  }
}

// TODO move to test-kit
object JOL {
  def netLayout(wrapper: AnyRef, wrapped: AnyRef = null): GraphLayout = {
    val wrapperLayout = GraphLayout.parseInstance(wrapper)
    if (wrapped eq null) wrapperLayout
    else wrapperLayout.subtract(GraphLayout.parseInstance(wrapped))
  }
  def assertTotalSize(expectedSize: Int, layout: GraphLayout): Unit = {
    Assert.assertEquals(layout.toFootprint + "\n\n", expectedSize, layout.totalSize)
  }
}
