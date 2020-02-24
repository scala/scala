import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = true
    s
  }

  def code = """
    |type anotherint = Int
    |val four: anotherint = 4
    |""".stripMargin
}
