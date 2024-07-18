//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.ifelse0.IfElseSpec])

package scala.async.run.ifelse0 {

  import org.junit.Test
  import org.junit.Assert._
  import language.{reflectiveCalls, postfixOps}

  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.tools.testkit.async.Async.{async, await}
  object TestUtil {
    import language.implicitConversions
    implicit def lift[T](t: T): Future[T] = Future.successful(t)
    def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  }
  import TestUtil._


  class TestIfElseClass {
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
      val result = block(async {
        if ({await(true); await(true)}) await(1) else ???
      })
      assertEquals(1, result)
    }
  }

}
