//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.lazyval.LazyValSpec])

package scala.async.run.lazyval {

  import org.junit.Test
  import org.junit.Assert._

  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.collection.mutable.ListBuffer
  import scala.tools.testkit.async.Async.{async, await}
  object TestUtil {
    import language.implicitConversions
    implicit def lift[T](t: T): Future[T] = Future.successful(t)
    def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  }
  import TestUtil._

  class LazyValSpec {
    @Test
    def lazyValAllowed(): Unit = {
      val result = block(async {
        var x = 0
        lazy val y = { x += 1; 42 }
        assert(x == 0, x)
        val z = await(1)
        val result = y + x
        assert(x == 1, x)
        identity(y)
        assert(x == 1, x)
        result
      })

      assertEquals(43, result)
    }

    @Test
    def localObject(): Unit = {
      val result = block(async {
        val log = ListBuffer[String]()
        object O {
          log += "O"
        }
        await(1)
        O
        await(1)
        O
        var i = 0
        while (i <= 2) {
          object W {
            log += "W(" + i + ")"
          }
          await(1)
          W
          await(1)
          W
          i += 1
        }
        log.mkString(",")
      })

      assertEquals("O,W(0),W(1),W(2)", result)
    }
  }

}
