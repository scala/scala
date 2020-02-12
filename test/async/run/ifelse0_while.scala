object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.ifelse0.WhileSpec])

package scala.async.run.ifelse0 {

  import org.junit.Test
  import org.junit.Assert._
  import scala.async.internal.AsyncId

  class WhileSpec {

    @Test
    def whiling1(): Unit = {
      import AsyncId._

      val result = async {
        var xxx: Int = 0
        var y = 0
        while (xxx < 3) {
          y = await(xxx)
          xxx = xxx + 1
        }
        y
      }
      assertEquals(2, result)
    }

    @Test
    def whiling2(): Unit = {
      import AsyncId._

      val result = async {
        var xxx: Int = 0
        var y = 0
        while (false) {
          y = await(xxx)
          xxx = xxx + 1
        }
        y
      }
      assertEquals(0, result)
    }

    @Test
    def nestedWhile(): Unit = {
      import AsyncId._

      val result = async {
        var sum = 0
        var i = 0
        while (i < 5) {
          var j = 0
          while (j < 5) {
            sum += await(i) * await(j)
            j += 1
          }
          i += 1
        }
        sum
      }
      assertEquals(100, result)
    }

    @Test
    def whileExpr(): Unit = {
      import AsyncId._

      val result = async {
        var cond = true
        while (cond) {
          cond = false
          await { 22 }
        }
      }
      assertEquals((), result)
    }

    @Test def doWhile(): Unit = {
      import AsyncId._
      val result = async {
        var b = 0
        var x = ""
        await(do {
          x += "1"
          x += await("2")
          x += "3"
          b += await(1)
        } while (b < 2))
        await(x)
      }
      assertEquals("123123", result)
    }

    @Test def whileAwaitCondition(): Unit = {
      import AsyncId._
      val result = async {
        var b = true
        while(await(b)) {
          b = false
        }
        await(b)
      }
      assertEquals(false, result)
    }

    @Test def doWhileAwaitCondition(): Unit = {
      import AsyncId._
      val result = async {
        var b = true
        do {
          b = false
        } while(await(b))
        b
      }
      assertEquals(false, result)
    }
  }

}
