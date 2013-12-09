import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Xprint-pos -Xprint:typer -Yrangepos -Ystop-after:typer -d ${testOutput.path}"

  override def code = """
package foo

import scala.reflect.ClassTag

class Foo[T: ClassTag]
""".trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}