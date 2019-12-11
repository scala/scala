import scala.tools.partest.ReplTest

import scala.tools.nsc.Settings
import scala.util.chaining._

object Test extends ReplTest {
  def code = ":imports"
  override def transformSettings(ss: Settings) = ss.tap(_.processArgumentString("-Yimports java.lang scala"))
  lazy val liner = raw"\s*\d*\) import ([\w.]+) .*".r
  override def normalize(line: String): String =
    line match {
      case liner(pkg) => pkg
      case _          => ""
    }
}
