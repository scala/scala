object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.noawait.NoAwaitSpec])

package scala.async.run.noawait {

  import scala.async.internal.AsyncId
  import AsyncId._
  import org.junit.Test
  import org.junit.Assert._

  class NoAwaitSpec {
    @Test
    def `async block without await`(): Unit = {
      def foo = 1
      assertEquals(foo, async {
        foo
        foo
      })
    }

    @Test
    def `async block without await 2`(): Unit = {
      assertEquals(1, async {
        def x = 0
        if (x > 0) 0 else 1
      })
    }

    @Test
    def `async expr without await`(): Unit = {
      def foo = 1
      assertEquals(foo, async(foo))
    }
  }

}
