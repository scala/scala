
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def extraSettings = "-usejavacp -Wunused:imports"
}
