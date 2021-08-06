package scala.collection.convert

import java.{util => ju}

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import scala.collection.JavaConverters._
import scala.collection.Set

@deprecated("Tests deprecated API", since="2.13")
class JSetWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3))
    val sSet: Set[Int] = jSet.asScala

    assertTrue(sSet.isInstanceOf[JavaCollectionWrappers.JSetWrapper[_]])
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
