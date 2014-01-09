import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Yinfer-argument-types -Xprint-pos -Xprint:typer -Yrangepos -Ystop-after:typer -d ${testOutput.path}"

  override def code = """
class A(a: Int = A.a)

object A {
  val a = 2
}

class B extends A {
 def this(a) = this()
}
  """.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}