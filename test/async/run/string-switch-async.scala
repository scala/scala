//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
    assertEquals(Some(""), testSwitch(""))
    assertEquals(Some("aa"), testSwitch("a"))
  }

  private def testSwitch(s: String) = optionally {
    s match {
      case "" => ""
      case p =>
        value(Some(p)) + p
    }
  }
}
