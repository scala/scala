import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-deprecation"

  def code = """
'\060'
def foo() { }
@annotation.nowarn def sshhh() { }
  """.trim
}
