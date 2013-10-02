import scala.tools.nsc._

import scala.language.{ reflectiveCalls }

object Test {
  class DryRun {
    val compiler = new Global(new Settings()) {
      lazy val test1 = new AnyRef
    }
  }

  def main(args: Array[String]) {
    new DryRun().compiler.test1
  }
}
