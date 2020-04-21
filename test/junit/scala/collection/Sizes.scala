package scala.collection

import org.junit._
import scala.collection.mutable.ListBuffer
import scala.tools.testing.JOL
import scala.tools.testing.JOL.assertTotalSize


object Sizes {
  def list: Int = 24
  def listBuffer: Int = 32

  def refArray(length:Int): Int = {
    (16 + (length+1) * 4) /8 * 8
  }
  def wrappedRefArray(length:Int): Int = refArray(length) + 16

}
class Sizes {
  @Test
  def list: Unit = {
    assertTotalSize(Sizes.list, JOL.netLayout(null :: Nil, Nil))
  }
  @Test
  def listBuffer: Unit = {
    assertTotalSize(Sizes.listBuffer, JOL.netLayout(new ListBuffer[String], Nil))
  }
  @Test
  def rawArray: Unit = {
    for (length <- 0 to 10) {
      assertTotalSize(Sizes.refArray(length), JOL.netLayout(new Array[String](length), Nil))
    }
  }
  @Test
  def wrappedArray2: Unit = {
    for (length <- 1 to 10) {
      assertTotalSize(Sizes.wrappedRefArray(length), JOL.netLayout(mutable.WrappedArray.make[String](new Array[String](length)), Nil))
    }
  }

  @Test
  def wrappedArray: Unit = {
    val wrapped = Array[String]("")
    assertTotalSize(16, JOL.netLayout(mutable.WrappedArray.make[String](wrapped), wrapped))
  }
}
