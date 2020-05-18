
package scala.concurrent

import org.junit.Test

import scala.tools.testkit.AssertUtil._
import scala.util.Try

class FutureTest {
  @Test
  def `bug/issues#10513 firstCompletedOf must not leak references`(): Unit = {
    import ExecutionContext.Implicits._
    val unfulfilled = Promise[AnyRef]()
    val quick = Promise[AnyRef]()
    val result = new AnyRef
    Future.firstCompletedOf(List(quick.future, unfulfilled.future)): Unit
    assertNotReachable(result, unfulfilled) {
      quick.complete(Try(result))
    }
  }
}
