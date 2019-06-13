import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Vprint-pos -Vprint:typer -Yrangepos -Ystop-after:typer -d ${testOutput.path} -cp ${testOutput.path}"

  override def code = """
    object X {
      D.aaaaa
      D.sssss[Int]
      D.ddddd(1)
      D.fffff[Int](1)
    }
  """.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}

import language.dynamics
object D extends Dynamic {
  def selectDynamic[T](nme: String): String = ???
  def applyDynamic[T](name: String)(value: T) = ???
}
