package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

/* Test Try's withFilter method, which was added along with the -Xfuture fix for SI-6455  */
@RunWith(classOf[JUnit4])
class TryTest {
  @Test
  def withFilterFail(): Unit = {
    val fail = for (x <- util.Try(1) if x > 1) yield x
    assert(fail.isFailure)
  }

  @Test
  def withFilterSuccess(): Unit = {
    val success1 = for (x <- util.Try(1) if x >= 1) yield x
    assertEquals(success1, util.Success(1))
  }

  @Test
  def withFilterFlatMap(): Unit = {
    val successFlatMap = for (x <- util.Try(1) if x >= 1; y <- util.Try(2) if x < y) yield x
    assertEquals(successFlatMap, util.Success(1))
  }

  @Test
  def withFilterForeach(): Unit = {
    var ok = false
    for (x <- util.Try(1) if x == 1) ok = x == 1
    assert(ok)
  }
}