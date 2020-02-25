package scala.tools.tastytest

import scala.util.Properties

object Dotc {
  def main(args: Array[String]): Unit = {
    val Array(out, src) = args
    val success = TastyTest.dotc(out, src)
    sys.exit(if (success) 0 else 1)
  }
}
