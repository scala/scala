import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.language.experimental.macros
    |def pos_impl(c: scala.reflect.macros.blackbox.Context): c.Expr[String] = {
    |  import c.universe._
    |  c.Expr[String](Literal(Constant(c.enclosingPosition.getClass.toString)))
    |}
    |def pos: String = macro pos_impl
    |pos
    |""".stripMargin.trim

  override def extraSettings: String = "-Yrangepos"
}
