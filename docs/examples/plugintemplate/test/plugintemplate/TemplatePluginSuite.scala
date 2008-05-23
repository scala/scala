package plugintemplate

import org.scalatest.Suite
import org.scalatest.Ignore

class TemplatePluginSuite extends Suite {
  def testName() {
    import scala.tools.nsc.{Global, Settings}
    import scala.tools.nsc.reporters.ConsoleReporter
    val settings = new Settings
    val compiler = new Global(settings, new ConsoleReporter(settings))
    val plugin = new TemplatePlugin(compiler)
    expect("plugintemplate") {
      plugin.name
    }
  }

  @Ignore
  def testFail() {
    expect(1) { 2 }
  }
}
