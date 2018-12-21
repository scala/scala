
import scala.tools.partest.ReplTest

object Test extends ReplTest {

  def code = """
    |:paste < END
    |def f(): Unit = {
    |  "hello"
    |  42
    |}
    |END
  """.stripMargin.trim
} 
