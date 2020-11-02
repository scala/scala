import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def transformSettings(ss: Settings) = {
    ss.feature.value = true
    ss
  }

  // From zinc's InteractiveConsoleInterfaceSpecification "should evaluate postfix op with a warning"
  override def code = "import scala.concurrent.duration._; val t = 1 second"
}
