package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class WrappedStringTest {

  @Test // scala/bug#11518
  def indexOf_nonChar(): Unit = {
    assertEquals(-1, new WrappedString("test").indexOf[Any]("not a Char")) // doesn't overflow
  }
}
