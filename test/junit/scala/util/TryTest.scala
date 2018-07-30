package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

/* Test Try's withFilter method, which was added along with the -Xfuture fix for scala/bug#6455  */
@RunWith(classOf[JUnit4])
class TryTest {
  @Test
  def withFilterFail(): Unit = {
    val fail = for (x <- Try(1) if x > 1) yield x
    assertTrue(fail.isFailure)
  }

  @Test
  def withFilterSuccess(): Unit = {
    val success1 = for (x <- Try(1) if x >= 1) yield x
    assertEquals(Success(1), success1)
  }

  @Test
  def withFilterFlatMap(): Unit = {
    val successFlatMap = for (x <- Try(1) if x >= 1; y <- Try(2) if x < y) yield x
    assertEquals(Success(1), successFlatMap)
  }

  @Test
  def withFilterForeach(): Unit = {
    var ok = false
    for (x <- Try(1) if x == 1) ok = x == 1
    assertTrue(ok)
  }

  @Test
  def filterIsCheap(): Unit = {
    Try(1).filter(_ > 1) match {
      case Failure(e: Success.UnsatisfiedPredicateException[_]) => assertEquals(0, e.getStackTrace.size)
    }
  }
}
