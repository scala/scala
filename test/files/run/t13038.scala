
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def extraSettings = s"${super.extraSettings} -Xsource:3"
}
