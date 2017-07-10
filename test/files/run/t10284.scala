
// run repl -i script -e expression
// The Runner normally requires -howtorun:repl to pass -e to REPL.

import scala.tools.partest.ReplTest
import scala.tools.nsc.{GenericRunnerSettings, Settings}

object Test extends ReplTest {

  def code = ""
  def script = testPath.changeExtension("script")

  override def transformSettings(s: Settings): Settings = {
    val grs = new GenericRunnerSettings(s.errorFn)
    s.copyInto(grs)

    //grs.processArgumentString(s"-i $script -e f(21)")
    grs.pastefiles.value = List(script.toAbsolute.path)
    grs.execute.value    = "f(21)"
    grs
  }
}
