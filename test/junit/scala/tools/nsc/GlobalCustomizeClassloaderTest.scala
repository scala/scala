package scala.tools.nsc

import java.io.Closeable

import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue, fail}
import org.junit.jupiter.api.Test

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.internal.util.{AbstractFileClassLoader, NoSourceFile}
import scala.reflect.io.{Path, VirtualDirectory}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class GlobalCustomizeClassloaderTest {
  // Demonstrate extension points to customise creation of the classloaders used to load compiler
  // plugins and macro implementations.
  //
  // A use case could be for a build tool to take control of caching of these classloaders in a way
  // that properly closes them before one of the elements needs to be overwritten.
  @Test def test(): Unit = {
    def closeUnexpected(): Nothing = {
      // Scalac won't call close on this when Global.close is called, as it assumes that we are in charge
      // of its lifecycle.
      throw new UnsupportedOperationException("unexpected call to close()!")
    }

    val g = new Global(new Settings) {
      override def findMacroClassLoader(): ClassLoader = new URLClassLoader(Nil, getClass.getClassLoader){
        override def close(): Unit = closeUnexpected()
      }
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
        new AbstractFileClassLoader(d, getClass.getClassLoader) with Closeable {
          override def close(): Unit = closeUnexpected()
        }
      }
    }
    g.settings.usejavacp.value = true
    g.settings.plugin.value = List("sample")
    new g.Run
    assertTrue(g.settings.log.value == List("typer"))

    val unit = new g.CompilationUnit(NoSourceFile)
    val context = g.analyzer.rootContext(unit)
    val typer = g.analyzer.newTyper(context)
    import g._
    SampleMacro.data = "in this classloader"
    val typed = typer.typed(q"scala.tools.nsc.SampleMacro.m")
    assertTrue(!reporter.hasErrors)
    typed match {
      case Typed(Literal(Constant(s: String)), _) => assertEquals(SampleMacro.data, s)
      case _ => fail()
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
