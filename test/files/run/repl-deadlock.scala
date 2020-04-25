import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.concurrent._
    |import scala.concurrent.duration._
    |import scala.concurrent.ExecutionContext.Implicits.global
    |
    |Await.result(Future(42), 1.second) // progression to not deadlocking
    |""".stripMargin
}
