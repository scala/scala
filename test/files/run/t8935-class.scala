import scala.tools.partest.SessionTest
import scala.tools.nsc.Settings

object Test extends SessionTest {
  override def transformSettings(s: Settings): Settings = {
    s.Yreplclassbased.value = true
    s
  }
}
