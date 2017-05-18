import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings
import scala.tools.nsc.GenericRunnerSettings
import scala.tools.nsc.settings.MutableSettings

object Test extends ReplTest {
  def scriptPath = testPath.changeExtension("script")
  override def transformSettings(s: Settings) = s match {
    case m: MutableSettings =>
      val t = new GenericRunnerSettings(s.errorFn)
      m copyInto t
      t processArgumentString s"-usejavacp -i $scriptPath"
      t
    case _ => s
  }

  def code = ""
}

