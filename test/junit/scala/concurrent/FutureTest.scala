
package scala.concurrent

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._
import scala.util.Try


@RunWith(classOf[JUnit4])
class FutureTest {
  @Test
  def `bug/issues#10513 firstCompletedOf must not leak references`: Unit = {
    import ExecutionContext.Implicits._
    val unfulfilled = Promise[AnyRef]
    val quick = Promise[AnyRef]
    val result = new AnyRef
    val first = Future.firstCompletedOf(List(quick.future, unfulfilled.future))
    assertNotReachable(result, unfulfilled) {
      quick.complete(Try(result))
    }
  }
}
