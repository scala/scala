
import scala.tools.partest.ScriptTest

object Test extends ScriptTest {
  override def extraSettings = s"${super.extraSettings} -Xlint"
  override def argv          = Seq("good", "news")
}
