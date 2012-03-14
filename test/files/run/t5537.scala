import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
List[Predef.type]()
List[scala.`package`.type]()
List[List.type]()
List[Set.type]()
  """
}
