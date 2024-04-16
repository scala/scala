//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.block0.AsyncSpec])

package scala.async.run.block0 {

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

    def m2(y: Int): Future[Int] = async {
      val f = m1(y)
      val x = await(f)
      x + 2
    }

    def m3(y: Int): Future[Int] = async {
      val f1 = m1(y)
      val x1 = await(f1)
      val f2 = m1(y + 2)
      val x2 = await(f2)
      x1 + x2
    }
  }


  class AsyncSpec {

    @Test
    def `simple await`(): Unit = {
      val o = new Test1Class
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      assertEquals(14, res)
    }

    @Test
    def `several awaits in sequence`(): Unit = {
      val o = new Test1Class
      val fut = o.m3(10)
      val res = Await.result(fut, 4 seconds)
      assertEquals(26, res)
    }
  }

}
