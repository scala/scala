import scala.tools.partest.ReplTest
// f-bounded types involved in LUBs can sometimes produce an ill-bounded LUB.
// ReplTest because some errors shadow others
object Test extends ReplTest {
  def code =
    """def foo(a: Boolean, b: List[Any], c: collection.mutable.ListBuffer[Any]) = if (a) b else c
      |List(List[Any](), collection.mutable.ListBuffer[Any]())
      |List(List(), collection.mutable.ListBuffer())
      |List(List(), Vector())
      |List(collection.mutable.Queue(), List())
    """.stripMargin
}
