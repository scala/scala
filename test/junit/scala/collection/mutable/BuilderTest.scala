
package scala.collection.mutable

import org.junit.Test
import org.junit.Assert.assertEquals

class BuilderTest {
  class TestBuilder extends Builder[String, String] {
    val stuff = ListBuffer.empty[String]
    def clear() = stuff.clear()
    def result() = stuff.mkString("hello, world: ", ",", "")
    def addOne(s: String) = { stuff.addOne(s); this }
    var expectedSize = 16
    override def sizeHint(size: Int) = expectedSize = size
  }

  @Test def `sizeHint is capped`: Unit = {
    val b = new TestBuilder
    val prototype = Nil
    b.sizeHint(prototype, delta = -1)
    assertEquals(0, b.expectedSize)
    b.addOne("tested!")
    assertEquals("hello, world: tested!", b.result())
  }
}
