package scala.collection

import org.junit.Assert._
import org.junit.Test
import scala.util.Random

/* Test for scala/bug#7614 */
class TraversableOnceTest {
  val list = List.fill(1000)(Random.nextInt(10000) - 5000)

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def copyToBuffer(): Unit = {
    val b1 = mutable.ArrayBuffer.empty[Int]
    list.copyToBuffer(b1)
    val b2 = mutable.ArrayBuffer.empty[Int]
    b2 ++= list
    assertEquals(b1, b2)
  }

}
