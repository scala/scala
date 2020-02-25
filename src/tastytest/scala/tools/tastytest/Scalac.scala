package scala.tools.tastytest

import scala.util.Properties

object Scalac {
  def main(args: Array[String]): Unit = {
    val Array(out, src, additionalArgs @ _*) = args
    val success = TastyTest.scalac(out, additionalArgs, src)
    sys.exit(if (success) 0 else 1)
  }
}
