import scala.tools.partest.ReplTest
import scala.tools.util.Javap

object Test extends ReplTest {
  override def extraSettings = "-Yrich-exceptions"
  def code = """
    |sys.SystemProperties.traceSourcePath setValue ""
    |def f = sys.error("hi mom")
    |f
    |lastException.show
  """.stripMargin
}
