package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ScriptRunnerTest {
  @Test
  def testEmptyScriptSucceeds: Unit = {
    val s = new GenericRunnerSettings(s => ())
    s.nc.value = true
    s.usejavacp.value = true

    // scala -nc -e ''
    assertTrue(ScriptRunner.runCommand(s, "", Nil))

    // scala -nc -save -e ''
    s.save.value = true
    assertTrue(ScriptRunner.runCommand(s, "", Nil))
  }
}
