package scala.collection

import org.junit._
import scala.collection.mutable.ListBuffer
import scala.tools.testing.JOL
import scala.tools.testing.JOL.assertTotalSize

class Sizes {
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
    assertTotalSize(16, JOL.netLayout(mutable.WrappedArray.make[String](wrapped), wrapped))
  }
}
