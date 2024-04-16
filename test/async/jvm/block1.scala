//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.block1.Block1Spec])

package scala.async.run.block1 {

  import language.{reflectiveCalls, postfixOps}
  import scala.concurrent.{Future, ExecutionContext, Await}
  import scala.concurrent.duration._
  import scala.tools.testkit.async.Async.{async, await}
  import org.junit.Test
  import org.junit.Assert._


  class Test1Class {

    import ExecutionContext.Implicits.global

    def m1(x: Int): Future[Int] = Future {
      x + 2
    }

    def m4(y: Int): Future[Int] = async {
      val f1 = m1(y)
      val f2 = m1(y + 2)
      val x1 = await(f1)
      val x2 = await(f2)
      x1 + x2
    }
  }

  class Block1Spec {

    @Test def `support a simple await`(): Unit = {
      val o = new Test1Class
      val fut = o.m4(10)
      val res = Await.result(fut, 2 seconds)
      assertEquals(26, res)
    }
  }

}
