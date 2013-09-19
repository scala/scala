import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:typer -Yrangepos -Xprint-pos -d " + testOutput.path
  override def show() = Console.withErr(System.out)(compile())

  override def code = """
class A {
  def f1 = Some("a")
  def f2 = new Some("b")
}"""
}
