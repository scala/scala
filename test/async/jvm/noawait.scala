//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.noawait.NoAwaitSpec])

package scala.async.run.noawait {

  import org.junit.Test
  import org.junit.Assert._

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

  class NoAwaitSpec {
    @Test
    def `async block without await`(): Unit = {
      def foo = 1
      assertEquals(foo, block(async {
        foo
        foo
      }))
    }

    @Test
    def `async block without await 2`(): Unit = {
      assertEquals(1, block(async {
        def x = 0
        if (x > 0) 0 else 1
      }))
    }

    @Test
    def `async expr without await`(): Unit = {
      def foo = 1
      assertEquals(foo, block(async(foo)))
    }
  }

}
