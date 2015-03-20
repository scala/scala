package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import language.postfixOps

@RunWith(classOf[JUnit4])
class IterableViewLikeTest {

  @Test
  def hasCorrectDropAndTakeMethods() {
    val iter = Iterable(1, 2, 3)

    import scala.language.postfixOps
    assertEquals(Iterable.empty[Int], iter.view take Int.MinValue force)
    assertEquals(Iterable.empty[Int], iter.view takeRight Int.MinValue force)
    assertEquals(iter, iter.view drop Int.MinValue force)
    assertEquals(iter, iter.view dropRight Int.MinValue force)
  }
}
