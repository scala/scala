package scala.collection

import org.junit.Assert._
import org.junit._
import org.openjdk.jol.info.GraphLayout

import scala.collection.mutable.ListBuffer

object Sizes {
  lazy val list:  Long = 24
  lazy val listBuffer: Long = 32

  def refArray(length:Int): Long = {
    (16 + (length+1) * 4) /8 * 8
  }
  def wrappedRefArray(length:Int): Long = refArray(length) + 24

}
class Sizes{
  @Test
  def list: Unit = {
    val parsed = GraphLayout.parseInstance(new ::("", Nil)).subtract(GraphLayout.parseInstance(Nil)).subtract(GraphLayout.parseInstance(""))
    assertEquals(parsed.toPrintable, Sizes.list, parsed.totalSize())
  }
  @Test
  def listBuffer: Unit = {
    val parsed = GraphLayout.parseInstance(new ListBuffer[String]).subtract(GraphLayout.parseInstance(Nil))
    assertEquals(parsed.toPrintable, Sizes.listBuffer, parsed.totalSize())
  }

  @Test
  def rawArray: Unit = {
    for (length <- 0 to 10) {
      val parsed = GraphLayout.parseInstance(new Array[String](length))
      assertEquals(parsed.toPrintable, Sizes.refArray(length), parsed.totalSize())
    }
  }
  @Test
  def wrappedArray: Unit = {
    for (length <- 1 to 10) {
      val parsed = GraphLayout.parseInstance(mutable.WrappedArray.make[String](new Array[String](length)))
      assertEquals(parsed.toPrintable, Sizes.wrappedRefArray(length), parsed.totalSize())
    }
  }

}
