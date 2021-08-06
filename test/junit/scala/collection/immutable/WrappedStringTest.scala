package scala.collection.immutable

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class WrappedStringTest {

  @Test // scala/bug#11518
  def indexOf_nonChar(): Unit = {
    assertEquals(-1, new WrappedString("test").indexOf("not a Char")) // doesn't overflow
  }
}
