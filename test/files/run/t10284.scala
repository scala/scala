
// run repl -i script -e expression
// The Runner normally requires -howtorun:repl to pass -e to REPL.

import scala.tools.partest.{ReplTest, Welcoming}
import scala.tools.nsc.{GenericRunnerSettings, Settings}

// Welcoming just fakes ReplTest into not stripping leading lines of output
// since REPL doesn't emit a header for -e
object Test extends ReplTest with Welcoming {

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
