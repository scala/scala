import scala.tools.partest._

object Test extends DirectTest {
  def depCode =
    """object A {
      |  def unapply(a: Int): true = true
      |}
    """.stripMargin

  override def code =
    """class T {
      |  def t: Any = 2 match {
      |    case A() => "ok"
      |    case _   => "other"
      |  }
      |}
    """.stripMargin

  def show(): Unit = Console.withErr(System.out) {
    compileString(newCompiler("-usejavacp"))(depCode)
    compileString(newCompiler("-usejavacp", "-cp", testOutput.path, "-Vprint:patmat"))(code)
  }
}
