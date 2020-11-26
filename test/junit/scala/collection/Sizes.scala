package scala.collection

import org.junit._
import scala.collection.mutable.ListBuffer
import org.openjdk.jol.info.GraphLayout

object Sizes {
  def list: Int = 24
  def listBuffer: Int = 32

  def refArray(length:Int): Int = {
    (16 + (length+1) * 4) /8 * 8
  }
  def wrappedRefArray(length:Int): Int = refArray(length) + 16
  def wrappedRefArrayIterator: Int = 24
}

class Sizes {
  import JOL._
  @Test
  def list(): Unit = {
    assertTotalSize(Sizes.list, JOL.netLayout(null :: Nil, Nil))
  }
  @Test
  def listBuffer(): Unit = {
    assertTotalSize(Sizes.listBuffer, JOL.netLayout(new ListBuffer[String], Nil))
  }
  @Test
  def rawArray(): Unit = {
    for (length <- 0 to 10) {
      assertTotalSize(Sizes.refArray(length), JOL.netLayout(new Array[String](length), Nil))
    }
  }
  @Test @deprecated("Tests deprecated API", since="2.13.3")
  def wrappedArray2(): Unit =
    for (length <- 1 to 10)
      assertTotalSize(Sizes.wrappedRefArray(length), JOL.netLayout(mutable.WrappedArray.make[String](new Array[String](length)), Nil))

  @Test
  def wrappedArray(): Unit = {
    val wrapped = Array[String]("")
    assertTotalSize(16, JOL.netLayout(mutable.ArraySeq.make[String](wrapped), wrapped))
    assertTotalSize(16, JOL.netLayout(immutable.ArraySeq.unsafeWrapArray[String](wrapped), wrapped))
  }
}

// TODO move to test-kit
object JOL {
  // Instruct JOL (jol/jol-core/src/main/java/org/openjdk/jol/vm/sa/ServiceabilityAgentSupport.java)
  // to skip trying to use the HotSpot servicability agent to precisely compute the architecture specifics of the
  // running JVM. JOL will do make educated guesses which should be fine for the platforms we're testing on.
  //
  // Avoids, for me at least:
  // # WARNING: Unable to attach Serviceability Agent. HotSpot Serviceability Agent attach skipped due to jol.skipHotspotSAAttach flag.
  // ..  sbt.ForkMain 61774 failed with exit code 137
  //
  System.setProperty("jol.skipHotspotSAAttach", "true")
  def netLayout(wrapper: AnyRef, wrapped: AnyRef = null): GraphLayout = {
    val wrapperLayout = GraphLayout.parseInstance(wrapper)
    if (wrapped eq null) wrapperLayout
    else wrapperLayout.subtract(GraphLayout.parseInstance(wrapped))
  }
  def assertTotalSize(expectedSize: Int, layout: GraphLayout): Unit = {
    Assert.assertEquals(layout.toFootprint + "\n\n", expectedSize, layout.totalSize)
  }
}
