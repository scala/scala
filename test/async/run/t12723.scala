//> using options -Xasync -Werror -Wnonunit-statement

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
    val r = optionally {
      value(Some(true))
    }
    assert(r.get)
  }
}
