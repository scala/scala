
import tools.nsc.plugins.PluginDescription
import tools.partest.DirectTest

import java.io.File

// show that plugins are on isolated class loaders
object Test extends DirectTest {
  override def code = "class Code"

  override def extraSettings = s"-usejavacp"

  // plugin named ploogin1_1 or ploogin1_2, but not ploogin2_x
  // Although the samples are in different classloaders, the plugin
  // loader checks for distinctness by class name, so the names must differ.
  def pluginCode(index: Int) = s"""
    |package t4841 {
    |  class SamplePloogin$index(global: scala.tools.nsc.Global) extends Ploogin(global, s"$${PlooginCounter.named}_$index")
    |  object PlooginCounter {
    |    val count = new java.util.concurrent.atomic.AtomicInteger
    |    def named = s"ploogin$${count.incrementAndGet}"
    |  }
    |}""".stripMargin.trim

  def compilePlugin(i: Int) = {
    val out  = (testOutput / s"p$i").createDirectory()
    val args = Seq("-usejavacp", "-d", out.path)
    compileString(newCompiler(args: _*))(pluginCode(i))
    val xml  = PluginDescription(s"p$i", s"t4841.SamplePloogin$i").toXML
    (out / "scalac-plugin.xml").toFile writeAll xml
    out
  }

  override def show() = {
    val dirs = 1 to 2 map (compilePlugin(_))
    compile("-Xdev", s"-Xplugin:${dirs mkString ","}", "-usejavacp", "-d", testOutput.path)
  }
}

