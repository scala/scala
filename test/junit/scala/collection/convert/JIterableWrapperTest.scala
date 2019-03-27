package scala.collection.convert

import java.{lang => jl, util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.Iterable
import scala.collection.JavaConverters._

@RunWith(classOf[JUnit4])
class JIterableWrapperTest {

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jIterable: jl.Iterable[Int] = ju.Arrays.asList(1, 2, 3)
    val sIterable: Iterable[Int] = jIterable.asScala

    assertTrue(sIterable.isInstanceOf[JavaCollectionWrappers.JIterableWrapper[_]])
    assertTrue(sIterable.iterator.sameElements(Iterable(1, 2, 3)))
  }
}
