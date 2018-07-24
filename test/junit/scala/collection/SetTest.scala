package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SetTest {

  @Test
  def intersectsSet(): Unit = {
    val s1 = Set(1, 2, 3)
    val s2 = Set(4, 5, 6)
    val s3 = Set(0, 3, 6, 8)
    assertFalse("These 2 sets have no intersection.", s1.intersects(s2))
    assertTrue("These 2 sets have a intersection.", s1.intersects(s3))
    assertTrue("These 2 sets have a intersection.", s2.intersects(s3))
  }
}
