package scala.collection.convert

import java.{util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import scala.collection.Iterable
import scala.collection.JavaConverters._

class JCollectionWrapperTest {

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jCol: ju.Collection[Int] = ju.Arrays.asList(1, 2, 3)
    val sIterable: Iterable[Int] = jCol.asScala

    assertTrue(sIterable.isInstanceOf[JavaCollectionWrappers.JCollectionWrapper[_]])
    assertTrue(sIterable.iterator.sameElements(Iterable(1, 2, 3)))
  }
}
