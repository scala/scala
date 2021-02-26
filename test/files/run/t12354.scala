
import scala.tools.nsc.Settings
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def transformSettings(ss: Settings) = {
    ss.deprecation.value = true
    ss
  }
}
