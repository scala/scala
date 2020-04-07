package scala.tools.testing

import org.junit.Assert
import org.openjdk.jol.info.GraphLayout

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
