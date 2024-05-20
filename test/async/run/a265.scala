//> using options -Xasync -Xsource:3-cross
//> abusing options -Xasync -Xsource:3-cross -Vprint:typer,async

import concurrent.*, ExecutionContext.Implicits.{given, *}, duration.Duration.Inf
import scala.tools.testkit.AssertUtil.assertNotReachable
import scala.tools.testkit.async.Async.*
import org.junit.Assert.*

object Test {
  def main(args: Array[String]): Unit = {
    val data = "hello, world"
    val r = async {
      //println(await(Future(data)))
      await(Future(data))
      "goodbye, all!" // check local var is null
    }
    assertNotReachable(data, r) {
      assertEquals("goodbye, all!", Await.result(r, Inf))
    }
  }
}
