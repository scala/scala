
import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest
import scala.util.chaining._

object Test extends ReplTest {
  override def transformSettings(ss: Settings) = ss.tap(_.deprecation.value = true)
  override def code =
    """|def f = {
       |  val x = 'abc
       |  val y = x.toString
       |  y
       |}""".stripMargin
}
