import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-deprecation"
  def code = """
    |@deprecated("foooo", "ReplTest version 1.0-FINAL") class Foo() {
    |  override def toString = "Bippy"
    |}
    |val f = new Foo
  """.stripMargin
}
