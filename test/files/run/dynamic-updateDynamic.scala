import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Vprint-pos -Vprint:typer -Yrangepos -Ystop-after:typer -cp ${testOutput.path}"

  override def code = """
    object X {
      val d = new D
      d.field = 10
      d.field
    }
  """.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}

import language.dynamics
class D extends Dynamic {
  def selectDynamic(name: String): Any = ???
  def updateDynamic(name: String)(value: Any): Unit = ???
}

