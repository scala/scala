package scala.collection.convert

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.JavaConverters._
import scala.collection.Set

@RunWith(classOf[JUnit4])
class JSetWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3))
    val sSet: Set[Int] = jSet.asScala

    assertTrue(sSet.isInstanceOf[Wrappers.JSetWrapper[_]])
    assertTrue(sSet.iterator.sameElements(Set(1, 2, 3)))
  }

  @Test
  def testFilterInPlace(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    val sSet: collection.mutable.Set[Int] = jSet.asScala

    sSet.filterInPlace(_ % 2 == 0)

    assertEquals(sSet, Set(2, 4, 6, 8, 10))
  }

}
