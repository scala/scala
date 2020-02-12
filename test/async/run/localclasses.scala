object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.neg.LocalClasses0Spec])

package scala.async.neg {

  import org.junit.Test
  import org.junit.Assert._
  import scala.async.internal.AsyncId

  class LocalClasses0Spec {
    @Test
    def localClassCrashIssue16(): Unit = {
      import AsyncId.{async, await}
      assertEquals(1, async {
        class B { def f = 1 }
        await(new B()).f
      })
    }

    @Test
    def nestedCaseClassAndModuleAllowed(): Unit = {
      import AsyncId.{await, async}
      assertEquals("bob", async {
        trait Base { def base = 0}
        await(0)
        case class Person(name: String) extends Base
        val fut = async { "bob" }
        val x = Person(await(fut))
        x.base
        x.name
      })
    }
  }

}
