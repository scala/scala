import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
    override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = true // TODO: drop when it's default true, to assert it works out the box
    s
  }

  def code = """
    |import scala.concurrent._
    |import scala.concurrent.duration._
    |import scala.concurrent.ExecutionContext.Implicits.global
    |
    |Await.result(Future(42), 1.second) // progression to not deadlocking
    |""".stripMargin
}
