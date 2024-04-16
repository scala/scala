//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
//import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
     lambda()
  }

  private def lambda() = {
    val l = optionally{
      value(Some(1))
      (a: String) => a.length
    }.get
    //assertEquals(1, l("a"))
  }
}
