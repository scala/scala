object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.ifelse0.IfElseSpec])

package scala.async.run.ifelse0 {

  import language.{reflectiveCalls, postfixOps}
  import scala.concurrent.{Future, ExecutionContext, Await}
  import scala.concurrent.duration._
  import scala.async.Async.{async, await}
  import org.junit.Test
  import org.junit.Assert._
  import scala.async.internal.AsyncId


  class TestIfElseClass {

    import ExecutionContext.Implicits.global

    def m1(x: Int): Future[Int] = Future {
      x + 2
    }

    def m2(y: Int): Future[Int] = async {
      val f = m1(y)
      var z = 0
      if (y > 0) {
        val x1 = await(f)
        z = x1 + 2
      } else {
        val x2 = await(f)
        z = x2 - 2
      }
      z
    }
  }


  class IfElseSpec {

    @Test def `support await in a simple if-else expression`(): Unit = {
      val o = new TestIfElseClass
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      assertEquals(14, res)
    }

    @Test def `await in condition`(): Unit = {
      import AsyncId.{async, await}
      val result = async {
        if ({await(true); await(true)}) await(1) else ???
      }
      assertEquals(1, result)
    }
  }

}
