package scala.collection.immutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert.assertEquals

import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnit4])
class ViewTest {

  @Test
  def viewsAreImmutable: Unit = {
    val lb = ListBuffer(1,2,3)
    val v = collection.immutable.View.from(lb)
    lb += 1
    assertEquals(IndexedSeq(1,2,3), v.toIndexedSeq)
  }

}
