object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.lazyval.LazyValSpec])

package scala.async.run.lazyval {

  import org.junit.Test
  import org.junit.Assert._
  import scala.async.internal.AsyncId._

  class LazyValSpec {
    @Test
    def lazyValAllowed(): Unit = {
      val result = async {
        var x = 0
        lazy val y = { x += 1; 42 }
        assert(x == 0, x)
        val z = await(1)
        val result = y + x
        assert(x == 1, x)
        identity(y)
        assert(x == 1, x)
        result
      }
      assertEquals(43, result)
    }
  }

}
