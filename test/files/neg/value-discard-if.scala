// scalac: -Werror -Wvalue-discard -Wnonunit-if:false

import scala.util.chaining._

final class UnusedTest {
  def body = 27
  def `single case is one-legged if`(i: Int): Unit = i match {
    case 42 => body           // nowarn
    case _ =>
  }
  def `empty case is required`(i: Int): Unit = i match {
    case 42 => body           // warn
  }
  def pr(i: Int) = i.tap(println)
  def `one-legged if does not warn`(i: Int): Unit =
    if (i > 0) pr(i) // nowarn
  def `also one-legged with empty else`(i: Int): Unit =
    if (i > 0) pr(i) else () // nowarn
}
