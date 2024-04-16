//> using options -Xasync
import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

// Scala.js compatible test suite for -Xasync that doesn't use Scala futures
object Test {
  def main(args: Array[String]): Unit = {
    stringSwitchBug()
  }

  private def stringSwitchBug() = {
    assertEquals(Some(true), optionally {
      val x: String = ""
      val as = List("x")
      val it = as.iterator
      var okay = false
      while (it.hasNext) {
        val x = it.next()
        val res = (x match {
          case "x" =>
            okay = value(Some(1)) == 1
            ()
          case _ => ()
        })
      }
      okay
    })
  }
}
