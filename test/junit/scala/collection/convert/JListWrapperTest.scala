package scala.collection.convert

import java.{util => ju}

import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.Buffer

@deprecated("Tests deprecated API", since="2.13")
class JListWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jList: ju.List[Int] = ju.Arrays.asList(1, 2, 3)
    val sList: Buffer[Int] = jList.asScala

    assertTrue(sList.isInstanceOf[JavaCollectionWrappers.JListWrapper[_]])
    assertTrue(sList.iterator.sameElements(List(1, 2, 3)))
  }
}
