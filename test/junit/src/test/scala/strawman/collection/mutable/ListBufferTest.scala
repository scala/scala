package strawman.collection.mutable

import strawman.collection.immutable.Nil

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListBufferTest {

  @Test
  def hasCorrectClear(): Unit = {
    val b = ListBuffer.empty[String]
    b += "a"
    Assert.assertTrue(b.sameElements("a" :: Nil))
    b.clear()
    Assert.assertEquals(ListBuffer.empty[String], b)
    b += "b"
    Assert.assertTrue(b.sameElements("b" :: Nil))

    val b2 = ListBuffer.empty[String]
    b2 += "a"
    val _ = b2.toList
    b2.clear()
    b2 += "b"
    Assert.assertTrue(b2.sameElements("b" :: Nil))
  }

}
