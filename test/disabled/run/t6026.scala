
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code =
"""|class Foo
   |:javap Foo
   |""".stripMargin
}
