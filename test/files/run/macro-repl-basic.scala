import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xmacros"
  def code = """
    |object Macros {
    |  object Shmacros {
    |    def macro foo(x: Int): Int = x
    |  }
    |  def macro bar(x: Int): Int = x
    |}; class Macros {
    |  def macro quux(x: Int): Int = x
    |}
    |
    |import Macros.Shmacros._
    |println(foo(2) + Macros.bar(2) * new Macros().quux(4))
    |""".stripMargin
}