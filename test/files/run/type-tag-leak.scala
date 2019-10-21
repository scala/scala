import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

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
      val intp = new scala.tools.nsc.interpreter.IMain(settings)

      try {
        intp.quietRun("object C { val data = new Array[Byte](16 * 1024 * 1024) }")
        intp.quietRun("identity(C.data); Test.repo()")
      } finally {
        intp.close()
      }
    }
  }
}
