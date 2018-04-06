package strawman.collection.convert

import java.{util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.JavaConverters._
import strawman.collection.immutable.List
import strawman.collection.mutable.Buffer

@RunWith(classOf[JUnit4])
class JListWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jList: ju.List[Int] = ju.Arrays.asList(1, 2, 3)
    val sList: Buffer[Int] = jList.asScala

    assertTrue(sList.isInstanceOf[Wrappers.JListWrapper[_]])
    assertTrue(sList.iterator().sameElements(List(1, 2, 3)))
  }
}
