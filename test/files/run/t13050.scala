import scala.tools.nsc._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xasync"
  def code =
    """import scala.tools.partest.async.OptionAwait._
      |println(optionally { value(Some(1)) + value(Some(2)) })""".stripMargin
}
