import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import language.experimental.macros
    |def pos_impl(c: reflect.macros.Context): c.Expr[String] =
    |  c.literal(c.enclosingPosition.getClass.toString)
    |def pos = macro pos_impl
    |pos
    |""".stripMargin.trim

  override def extraSettings: String = "-Yrangepos"
}
