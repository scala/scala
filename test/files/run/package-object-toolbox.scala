import java.io.File
import java.net.URLClassLoader

import scala.reflect.io.Path
import scala.reflect.runtime.{ universe => ru }
import scala.tools.partest._
import scala.tools.reflect.ToolBox

import org.junit.Assert._

object Test extends StoreReporterDirectTest {
  val cp = List(sys.props("partest.lib"), testOutput.path)
  override def extraSettings = s"-cp ${cp.mkString(File.pathSeparator)}"

  def show(): Unit = {
    compiles("package object pkg { def foo = 1 }")
    val loader = new URLClassLoader(cp.map(new File(_).toURI.toURL).toArray)
    val mirror = ru.runtimeMirror(loader)

    val toolbox = mirror.mkToolBox()
    val result1 = toolbox.eval(toolbox.parse("pkg.foo"))
    assertEquals(1, result1)

    val obj  = toolbox.eval(toolbox.parse("pkg.`package`"))
    val pkg  = mirror.staticPackage("pkg")
    val sym  = pkg.info.decl(ru.TermName("foo")).asMethod
    val meth = mirror.reflect(obj).reflectMethod(sym)
    val res2 = meth.apply()
    assertEquals(1, res2)
  }

  def compiles(codes: String*) = {
    val global = newCompiler()
    withRun(global)(_ compileSources newSources(codes: _*))
    assert(!global.reporter.hasErrors, storeReporter.infos.mkString("\n"))
  }

  def delete(paths: Path*) = paths.foreach(p => assert(p.delete(), s"$p didn't delete"))
  def code = ""
}
