package scala.collection.convert

import java.{util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.Iterable
import scala.collection.JavaConverters._

@RunWith(classOf[JUnit4])
class JCollectionWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jCol: ju.Collection[Int] = ju.Arrays.asList(1, 2, 3)
    val sIterable: Iterable[Int] = jCol.asScala

    assertTrue(sIterable.isInstanceOf[Wrappers.JCollectionWrapper[_]])
    assertTrue(sIterable.iterator.sameElements(Iterable(1, 2, 3)))
  }
}
