package scala.tools.nsc

import org.junit.jupiter.api.Test

class ScriptRunnerTest {
  @Test
  def testEmptyScriptSucceeds(): Unit = {
    val s = new GenericRunnerSettings(s => ())
    s.usejavacp.value = true

    // scala -e ''
    ScriptRunner(s).runScriptText("", Nil).foreach(throw _)

    // scala -save -e ''
    s.save.value = true
    ScriptRunner(s).runScriptText("", Nil).foreach(throw _)
  }
}
