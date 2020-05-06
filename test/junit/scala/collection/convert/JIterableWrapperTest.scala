package scala.collection.convert

import java.{lang => jl, util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import scala.collection.Iterable
import scala.collection.JavaConverters._

class JIterableWrapperTest {

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jIterable: jl.Iterable[Int] = ju.Arrays.asList(1, 2, 3)
    val sIterable: Iterable[Int] = jIterable.asScala

    assertTrue(sIterable.isInstanceOf[JavaCollectionWrappers.JIterableWrapper[_]])
    assertTrue(sIterable.iterator.sameElements(Iterable(1, 2, 3)))
  }
}
