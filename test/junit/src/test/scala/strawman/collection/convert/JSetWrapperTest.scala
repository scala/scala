package strawman.collection.convert

import java.{util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.JavaConverters._
import strawman.collection.Set

@RunWith(classOf[JUnit4])
class JSetWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3))
    val sSet: Set[Int] = jSet.asScala

    assertTrue(sSet.isInstanceOf[Wrappers.JSetWrapper[_]])
    assertTrue(sSet.iterator().sameElements(Set(1, 2, 3)))
  }
}
