import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-deprecation"
  def code =
    """@deprecated(message = "x", since = "y") def f = 1; f
      |:pa -raw << JUMP!
      |package scala { class deprecated(message: String = "", since: String = "") extends scala.annotation.ConstantAnnotation }
      |JUMP!
      |@deprecated(message = "x", since = "y") def g = 1; g
      |""".stripMargin
}
