import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Vprint-pos -Vprint:typer -Yrangepos -Ystop-after:typer -cp ${testOutput.path}"

  override def code = """
    object Test {
      private[this] val n = 1

      1 foo_: C
      1 foo_:[Any] C

      n foo_: C
      n foo_:[Any] C

      1 bar_: C
      1 bar_:[Any] C

      n bar_: C
      n bar_:[Any] C
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
  def bar_:[Dummy](i:    Int): Int = i
}
object C extends C
