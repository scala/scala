//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.neg.LocalClasses0Spec])

package scala.async.neg {

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

  class LocalClasses0Spec {
    @Test
    def localClassCrashIssue16(): Unit = {
      assertEquals(1, block(async {
        class B { def f = 1 }
        await(new B()).f
      }))
    }

    @Test
    def nestedCaseClassAndModuleAllowed(): Unit = {
      assertEquals("bob", block(async {
        trait Base { def base = 0}
        await(0)
        case class Person(name: String) extends Base
        val fut = async { "bob" }
        val x = Person(await(fut))
        x.base
        x.name
      }))
    }
  }

}
