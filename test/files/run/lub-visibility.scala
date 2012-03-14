import scala.tools.partest.ReplTest
object Test extends ReplTest {
  def code = """
    |// should infer List[scala.collection.immutable.Seq[Nothing]]
    |// but reverted that for SI-5534.
    |val x = List(List(), Vector())
  """.stripMargin
}
