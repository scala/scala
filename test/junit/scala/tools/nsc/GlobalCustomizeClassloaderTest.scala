package scala.tools.nsc

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.{AbstractFileClassLoader, NoSourceFile}
import scala.reflect.io.{Path, VirtualDirectory}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

@RunWith(classOf[JUnit4])
class GlobalCustomizeClassloaderTest {
  // Demonstrate extension points to customise creation of the classloaders used to load compiler
  // plugins and macro implementations.
  //
  // A use case could be for a build tool to take control of caching of these classloaders in a way
  // that properly closes them before one of the elements needs to be overwritten.
  @Test def test(): Unit = {
    val g = new Global(new Settings) {
      override protected def findPluginClassLoader(classpath: Seq[Path]): ClassLoader = {
        val d = new VirtualDirectory("", None)
        val xml = d.fileNamed("scalac-plugin.xml")
        val out = xml.bufferedOutput
        out.write(
          s"""<plugin>
            |<name>sample-plugin</name>
            |<classname>${classOf[SamplePlugin].getName}</classname>
            |</plugin>
            |""".stripMargin.getBytes())
        out.close()
        new AbstractFileClassLoader(d, getClass.getClassLoader)
      }
    }
    g.settings.usejavacp.value = true
    g.settings.plugin.value = List("sample")
    new g.Run
    assert(g.settings.log.value == List("typer"))

    val unit = new g.CompilationUnit(NoSourceFile)
    val context = g.analyzer.rootContext(unit)
    val typer = g.analyzer.newTyper(context)
    import g._
    SampleMacro.data = "in this classloader"
    val typed = typer.typed(q"scala.tools.nsc.SampleMacro.m")
    assert(!reporter.hasErrors)
    typed match {
      case Typed(Literal(Constant(s: String)), _) => Assert.assertEquals(SampleMacro.data, s)
      case _ => Assert.fail()
    }
    g.close()
  }
}

object SampleMacro {
  var data: String = _
  import language.experimental.macros
  import scala.reflect.macros.blackbox.Context
  def m: String = macro impl
  def impl(c: Context): c.Tree = c.universe.Literal(c.universe.Constant(data))
}

class SamplePlugin(val global: Global) extends Plugin {
  override val name: String = "sample"
  override val description: String = "sample"
  override val components: List[PluginComponent] = Nil
  override def init(options: List[String], error: String => Unit): Boolean = {
    val result = super.init(options, error)
    global.settings.log.value = List("typer")
    result
  }
}
