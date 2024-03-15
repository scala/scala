// java: -Xmx192M -XX:+ExitOnOutOfMemoryError

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.tools.nsc.interpreter._
import java.io._

class Test {

  def repo(): String = {
    val tag = implicitly[TypeTag[Array[Byte]]]
    val mirror = universe.runtimeMirror(Thread.currentThread().getContextClassLoader)
    tag.in(mirror).tpe.dealias.toString
  }
}

object Test extends Test {

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 16) {
      val settings = new scala.tools.nsc.Settings
      settings.Xnojline.value = true
      settings.usejavacp.value = true

      val intp = new IMain(settings, new shell.ReplReporterImpl(settings, new PrintWriter(new StringWriter)))

      try {
        intp.quietRun("object C { val data = new Array[Byte](16 * 1024 * 1024) }")
        intp.quietRun("identity(C.data); Test.repo()")
      } finally {
        intp.close()
      }
    }
  }
}
