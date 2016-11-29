import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

object Test extends ReplTest {
  override def transformSettings(settings: Settings): Settings = {
    settings.nopredef.value = true
    settings
  }  
  def code = ":implicits"
}
