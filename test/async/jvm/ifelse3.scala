//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.ifelse3.IfElse3Spec])

package scala.async.run.ifelse3 {

  import language.{reflectiveCalls, postfixOps}
  import scala.concurrent.{Future, ExecutionContext, Await}
  import scala.concurrent.duration._
  import scala.tools.testkit.async.Async.{async, await}
  import org.junit.Test
  import org.junit.Assert._


  class TestIfElse3Class {

    import ExecutionContext.Implicits.global

    def base(x: Int): Future[Int] = Future {
      x + 2
    }

    def m(y: Int): Future[Int] = async {
      val f = base(y)
      var z = 0
      if (y > 0) {
        val x1 = await(f)
        var w = x1 + 2
        z = w + 2
      } else {
        val x2 = await(f)
        var w = x2 + 2
        z = w - 2
      }
      z
    }
  }


  class IfElse3Spec {

    @Test
    def `variables of the same name in different blocks`(): Unit = {
      val o = new TestIfElse3Class
      val fut = o.m(10)
      val res = Await.result(fut, 2 seconds)
      assertEquals(16, res)
    }
  }

}
