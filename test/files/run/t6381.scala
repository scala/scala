import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import language.experimental.macros
    |def pos_impl(c: reflect.macros.BlackboxContext): c.Expr[String] = {
    |  import c.universe._
    |  c.Expr[String](Literal(Constant(c.enclosingPosition.getClass.toString)))
    |}
    |def pos = macro pos_impl
    |pos
    |""".stripMargin.trim

  override def extraSettings: String = "-Yrangepos"
}
