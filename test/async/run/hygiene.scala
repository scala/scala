object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.hygiene.HygieneSpec])

package scala.async.run.hygiene {

  import org.junit.Test
  import org.junit.Assert._
  import scala.async.internal.AsyncId

  class HygieneSpec {

    import AsyncId.{async, await}

    @Test
    def `is hygenic`(): Unit = {
      val state = 23
      val result: Any = "result"
      def resume(): Any = "resume"
      val res = async {
        val f1 = state + 2
        val x  = await(f1)
        val y  = await(result)
        val z  = await(resume())
        (x, y, z)
      }
      assertEquals((25, "result", "resume"), res)
    }

    @Test
    def `external var as result of await`(): Unit = {
      var ext = 0
      async {
        ext = await(12)
      }
      assertEquals(12, ext)
    }

    @Test
    def `external var as result of await 2`(): Unit = {
      var ext = 0
      val inp = 10
      async {
        if (inp > 0)
          ext = await(12)
        else
          ext = await(10)
      }
      assertEquals(12, ext)
    }

    @Test
    def `external var as result of await 3`(): Unit = {
      var ext = 0
      val inp = 10
      async {
        val x = if (inp > 0)
          await(12)
        else
          await(10)
        ext = x + await(2)
      }
      assertEquals(14, ext)
    }

    @Test
    def `is hygenic nested`(): Unit = {
      val state = 23
      val result: Any = "result"
      def resume(): Any = "resume"
      import AsyncId.{await, async}
      val res = async {
        val f1 = async { state + 2 }
        val x  = await(f1)
        val y  = await(async { result })
        val z  = await(async(await(async { resume() })))
        (x, y, z)
      }
      assertEquals(25, res._1)
      assertEquals("result", res._2)
      assertEquals("resume", res._3)
    }
  }

}
