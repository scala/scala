import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Xprint-pos -Xprint:typer -Yrangepos -Ystop-after:typer -d ${testOutput.path} -cp ${testOutput.path}"

  override def code = """
    object Test {
      1 foo_: C
      1 foo_:[Any] C
    }
  """.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}

class C {
  def foo_:[Dummy](i: => Int): Int = i
  def t(c: C) = 1 foo_: c
  def u(c: C) = 1 foo_:[Any] c
}
object C extends C
