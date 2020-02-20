import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = false // test object-based encoding
    s
  }

  def code = """
    |case class Name(value: String)
    |val x = Name("foo")
    |val y = Name("bar")
    |val z = Name(x.value + y.value)
    |""".stripMargin
}
