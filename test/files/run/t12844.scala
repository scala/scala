
import scala.tools.partest.SessionTest

// .check file is the session text to verify
object Test extends SessionTest {
  override def extraSettings: String = "-usejavacp -Xlint"
}
